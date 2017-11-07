open Mods
open Tools
open Ast

	
let agents_of_complex sg emb env =
	let roots = match emb with State.CONNEX e | State.DISJOINT e | State.AMBIGUOUS e -> e.State.roots in
	let root = Graph.SiteGraph.node_of_id sg (IntSet.choose roots)
	and hsh_lnk = Hashtbl.create 10 in
	let rec iter_graph node fresh visited_list =
		if List.mem (Node.get_address node) visited_list then 
			(*Debug.tag ((string_of_list string_of_int visited_list) ^ " # " ^ (string_of_int (Node.get_address node)));*)
			([],fresh,visited_list)
		else
			let visited = Node.get_address node :: visited_list 
			and kappa_ag,fresh' = Node.to_string false (hsh_lnk,fresh) node env in
			(*let kappa_ag = (kappa_ag ^ "[" ^ (string_of_int (Node.get_address node)) ^ "]") in*)
			Array.fold_left (fun (kappa_list,fresh,visited') intf ->
				match intf.Node.status with
					| _, Node.Null -> kappa_list,fresh,visited'
					| _, Node.Ptr (node',p) ->
						let ka_lst,frsh,vstd = iter_graph node' fresh visited' in
							kappa_list @ ka_lst,frsh,vstd
					| _, Node.FPtr (n,m) -> 
						Debug.tag "Error: not valid node status (iter_graph)";
						kappa_list,fresh,visited'
				) ([kappa_ag],fresh',visited) (Node.interface node)	
	in iter_graph root 1 []

let agents_of_script script sg emb env =
	let hsh_lnk = Hashtbl.create 10
	and phi = State.map_of emb in
	let rec iter script agents fresh =
	match script with
	| [] -> 
		agents
	| action :: script' ->
		match action with
		| Dynamics.DEL i ->
			let phi_i =
				(try IntMap.find i phi	with Not_found ->	invalid_arg "Transport.apply: incomplete embedding 3") 
			in
				let node_i = Graph.SiteGraph.node_of_id sg phi_i in
				let kappa_ag,fresh' = Node.to_string false (hsh_lnk,fresh) node_i env
				in iter script' (kappa_ag :: agents) fresh'
		| _ -> Debug.tag "Error in transport"; exit 1
	in iter script [] 1

let string_to_astmixt s =
	let agents = Str.split (Str.regexp ";") s in
	List.fold_left (fun astmixt agent ->
		let splited = Str.split (Str.regexp "[)(]") agent in
		let	ag_name,sites = (List.hd splited) , Str.split (Str.regexp ",") (List.nth splited 1) in
		Ast.COMMA ({
			Ast.ag_nme = ag_name;
			Ast.ag_pos = Tools.no_pos;
			Ast.ag_intf = 
				let rec iter sites =
					if sites = [] then Ast.EMPTY_INTF
					else
						let site_lnk = Str.split (Str.regexp "!") (List.hd sites) in
						let site_val = Str.split (Str.regexp "~") (List.hd site_lnk)
						in
						Ast.PORT_SEP (
							{	Ast.port_nme = List.hd site_val;
								Ast.port_lnk = 
									if List.length site_lnk = 2 then 
										Ast.LNK_VALUE (int_of_string (List.nth site_lnk 1),Tools.no_pos) 
									else Ast.FREE;
								Ast.port_pos = Tools.no_pos;
								Ast.port_int = 
									if List.length site_val = 2 then 
										[List.nth site_val 1]
									else [];
							} , iter (List.tl sites)
						)
				in
					iter sites
		}, astmixt )
	) Ast.EMPTY_MIX agents

let pert_of_transport_string agent_str num arr_time counter from env =
	try
	if num = 0 then
		None,None,env
	else begin
	let agent,env = Eval.mixture_of_ast None false env (string_to_astmixt agent_str) in
	let str_pert = Printf.sprintf "Transport %d %s at %.15f (%d,%d)" 
		num (Mixture.to_kappa false agent env) arr_time (Counter.get_sync_count counter) from in
	(*Debug.tag (Printf.sprintf "%d: %s"  (Mpi.comm_rank Mpi.comm_world) (Mixture.to_kappa false agent env));*)
	let env,p_id = Environment.declare_pert (str_pert,Tools.no_pos) env in
	let (env, id) =	Environment.declare_var_kappa None env in
	let lhs = Mixture.empty (Some id) in
	let r_id = Mixture.get_id lhs in
	let (script,balance,added,modif_sites) = 
		Dynamics.diff Tools.no_pos lhs agent (Some (str_pert,Tools.no_pos)) env in
	let env = Environment.declare_rule (Some (str_pert,Tools.no_pos)) r_id env in
	
	let env = Environment.add_dependencies Mods.TIME (Mods.PERT p_id) env
	in
	
	let pre_causal = Dynamics.compute_causal lhs agent script env in 
	let rule = 
		{
			Dynamics.rm_token = [] ; Dynamics.add_token = [] ; 
			Dynamics.k_def = Dynamics.CONST (Num.F 0.0);
			Dynamics.k_alt = (None,None);
			Dynamics.over_sampling = None;
			Dynamics.script = script ;
			Dynamics.kappa = (Mixture.to_kappa false agent env);
			Dynamics.balance = balance;
			Dynamics.refines = None;
			Dynamics.lhs = Mixture.empty (Some id);
			Dynamics.rhs = agent;
			Dynamics.r_id = r_id;
			Dynamics.added = List.fold_left (fun set i -> IntSet.add i set) IntSet.empty added ;
			(*Dynamics.side_effect = side_effect ; *)
			Dynamics.modif_sites = modif_sites ;
			Dynamics.is_pert = true ;
			Dynamics.pre_causal = pre_causal ;
			Dynamics.cc_impact = None ;
			Dynamics.transport_to = None
		}
	in 
	let effect = Dynamics.INTRO (Dynamics.CONST (Num.F (float_of_int num)), agent)
	in
	(*let (x, is_constant, dep, str_pre) =
		((fun _ _ _ _ _ _ _-> true), true, DepSet.singleton Mods.EVENT, "true")
	in*)
	let bv = 
		let new_arr_time = if (arr_time > 0.0) then arr_time else counter.Counter.time in
		let (x, is_constant, dep, str_pre,_) = Eval.partial_eval_bool env 
			(Ast.OR (
			(Ast.GREATER ( Ast.TIME_VAR Tools.no_pos, Ast.FLOAT (new_arr_time,Tools.no_pos), Tools.no_pos) ),
			(Ast.EQUAL ( Ast.TIME_VAR Tools.no_pos, Ast.FLOAT (new_arr_time,Tools.no_pos), Tools.no_pos) ),
			Tools.no_pos))
			
		in
			if is_constant then 
				Dynamics.BCONST (Dynamics.close_var x)
			else 
				Dynamics.BVAR x
	in
	let pert = 
		{ Dynamics.precondition = bv;
			Dynamics.effect = [ (Some rule, effect) ];
			Dynamics.abort = None;
			Dynamics.flag = str_pert;
			Dynamics.stopping_time = None;
		}
	in
		(Some (p_id,pert), Some rule, env)
	end
	with
	| ex -> Debug.tag (Printf.sprintf "ERROR-> Transport %d %s at %.6f (%d)"
		num agent_str arr_time (Counter.get_sync_count counter) );
		Debug.tag (Printexc.to_string ex);
		None,None,env
	(** **)



			
			

let perts_of_transports transports counter env =	
	let (_,pert_list,rule_list,env) = 
	Array.fold_left (fun (i,p,e,env) transport_compart ->
		match transport_compart with
			| None -> (i+1,p,e,env)
			| Some transport_map ->
			let ps,es,env = 
				Hashtbl.fold (fun mixt_str (late_count,late_sum,arrival_list) (ps,es,env) ->
					(*Debug.tag (Printf.sprintf "%d: [%s] Llegan %d atrasados y %d adelantados"
						(Mpi.comm_rank Mpi.comm_world) mixt_str late_count ((List.length arrival_list)) );*)
					let (perts,efcts,env) = (* Transport every agent at its time *)
						List.fold_left (fun (ps,es,env) arriv_time -> 
							let (p,e,env) = pert_of_transport_string mixt_str 1 arriv_time counter i env
							in match p,e with 
								| None,_ | _,None   -> (ps,es,env)
								| Some p,Some e -> (p::ps,e::es,env) 
						) (ps,es,env) (arrival_list)
					in
					(* Transport Q agent at sync time *)
					let (p,e,env) = pert_of_transport_string mixt_str (late_count) (Counter.get_next_synctime counter) counter i env
					in 
						match p,e with 
							| None,_ | _,None  -> (perts,efcts,env)
							| Some p,Some e -> (p::perts,e::efcts,env)
				) transport_map ([],[],env)
			in
				(i+1,p @ ps, e @ es, env)
	) (0,[],[],env) transports
	in (pert_list,rule_list),env

let apply_transport_effects state r kappa_cmpx counter env comp_map = 
	match r.Dynamics.transport_to with 
	| Some (comp,travel_t,joined) ->
		(*Debug.tag (Printf.sprintf "%d:transportando hacia %d \t %s %f" (Mpi.comm_rank Mpi.comm_world) (Hashtbl.find comp_map comp) r.Dynamics.kappa travel_t);
		Debug.tag kappa_cmpx;*)
		(* Add transporting complex to struct of transports*)
		let comp_table = (*Find destination compartment entry*)
			try Hashtbl.find state.State.transports comp
			with Not_found -> 
				Hashtbl.add state.State.transports comp (Hashtbl.create 5);
				Hashtbl.find state.State.transports comp
		in 
		let late_count,late_sum,arrival_list = ( (*find complex entry*)
			let lc,ls,al = 
			try Hashtbl.find comp_table kappa_cmpx
			with Not_found -> 
				Hashtbl.add comp_table kappa_cmpx (0,0.0,[]);
				Hashtbl.find comp_table kappa_cmpx
			in ref lc, ref ls, ref al
		) in
		let arrive_time = (Counter.time counter) +. travel_t in
		(* late transport *)
		if arrive_time < (Counter.get_next_synctime counter) then
			let late_time = (Counter.get_next_synctime counter) -. arrive_time in 
			late_count :=  !late_count + 1;
			late_sum := !late_sum +. late_time
		else( (*not late*)
			arrival_list := !arrival_list @ [arrive_time]
		);
		Hashtbl.replace comp_table kappa_cmpx (!late_count,!late_sum,!arrival_list);
		Hashtbl.replace state.State.transports comp comp_table;
		state
	| None -> state


let apply state (r,(comp,travel,joined)) embedding_t counter env comp_map =
	let r,kappa_cmpx = 
	if joined then
		let kappa_list,_,node_list = agents_of_complex (state.State.graph) embedding_t env in
		let new_script = List.fold_right (fun id sc -> (Dynamics.DEL id) :: sc) node_list []
		in {r with Dynamics.script = new_script},String.concat ";" kappa_list
	else
		let kappa_list = agents_of_script r.Dynamics.script state.State.graph embedding_t env 
		in r,String.concat ";" kappa_list
	in
	(*Debug.tag (kappa_cmpx);*)
	let rec edit state script psi side_effects pert_ids env =
		(* phi: embedding, psi: fresh map *)
		let sg = state.State.graph
		and phi = State.map_of embedding_t
		in
		match script with
		| [] -> 
			let state = apply_transport_effects state r kappa_cmpx counter env comp_map
			in (env,state, (side_effects:Int2Set.t), embedding_t, psi, pert_ids)
		| action :: script' ->
			match action with
			| Dynamics.DEL i ->
				let phi_i =
					if joined then
						i
					else
						(try IntMap.find i phi	with Not_found ->	invalid_arg "Transport.apply: incomplete embedding 3") 
				in
					let node_i = Graph.SiteGraph.node_of_id sg phi_i in
					let env,side_effects,pert_ids = State.delete state r.Dynamics.r_id node_i side_effects pert_ids counter env
					in
					Graph.SiteGraph.remove sg phi_i;
					(*let hsh_lnk = Hashtbl.create 10 in
					let kappa,_ = Node.to_string false (hsh_lnk,1) node_i env in
					Debug.tag ("removing "^ kappa );*)
					edit state script' psi side_effects pert_ids env
			| _ -> Debug.tag "Error in transport"; exit 1
	in edit state r.Dynamics.script IntMap.empty Int2Set.empty IntSet.empty env


let add_perturbations (pert_list,rule_list) state =
	let perts = 
		List.fold_left (fun (perts) (p_id,pert) ->
			IntMap.add p_id pert perts
		) (state.State.perturbations) pert_list
	and rules,_ =
		List.fold_left (fun (rs,i) r ->
			Hashtbl.add rs (r.Dynamics.r_id) r; rs,i+1
		) (state.State.rules, Hashtbl.length state.State.rules) rule_list
	in
	{state with State.rules=rules; State.perturbations = perts}

