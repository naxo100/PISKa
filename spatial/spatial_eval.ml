open Mods
open Tools
open Ast
open Linear
open Spatial_util


(** returns the arity of an Ast.mixture *)
let arity_of_astmix astmix =
	let rec iter_ag astmix ag_links =
		match astmix with
			| Ast.COMMA (ag,mx) -> 
				let rec iter_intf intf ids =
					match intf with
						| Ast.PORT_SEP (prt,intf') ->
							(match prt.Ast.port_lnk with
								| Ast.LNK_VALUE (l_id,_) ->
									iter_intf intf' (l_id::ids)
								| _ -> 
									iter_intf intf' ids
							)
						| Ast.EMPTY_INTF -> ids
				in  iter_ag mx ((iter_intf ag.Ast.ag_intf [] ):: ag_links)
			| Ast.EMPTY_MIX -> ag_links
	in 
	let ag_links = iter_ag astmix [] in
	let rec find_arity edges left_nodes past_nodes count =
		let nodes = left_nodes @ past_nodes in
		if edges = [] && left_nodes != [] then
			find_arity (List.hd nodes) (List.tl nodes) [] count + 1 
		else if nodes = [] then
			count + 1
		else
			let edge = (List.hd edges)
			and node = (List.hd nodes) in
			if (List.mem edge node) then
				(find_arity
					(List.filter (fun edge2 -> edge != edge2 ) ( edges @ node))
					(past_nodes @ (List.tl left_nodes))
					[]
					count
				)
			else
				(find_arity
					edges
					(List.tl left_nodes)
					(node :: past_nodes )
					count
				)
	in 
		if ag_links = [] then 
			0
		else
			find_arity (List.hd ag_links) (List.tl ag_links) [] 0



(** ?(string * int) list -> index_expr -> int * string list
 * Returns (int,[]) if the index-expression can be evaluated
 * with the given (var-name-> value) assoc list, or 
 * (-1,string list) if expression can not be evaluated.
 * string list contain all literals encountered in evaluation, without repeats.
 *)
let partial_eval_index ?(vars=[]) ?(dim=None) index =
	let r_index,r_vars,pos = 
		let rec eval_rec index =
			match index with
			| INT_I (i,pos) -> Some i,[],pos
			| NAME (s,pos) ->(
				try Some (List.assoc s vars),[s],pos
				with Not_found -> None,[s],pos
				)
			| MULT_I (index1,index2,pos)
			| SUM_I (index1,index2,pos)
			| DIV_I (index1,index2,pos)
			| MINUS_I (index1,index2,pos)
			| POW_I (index1,index2,pos)
			| MODULO_I (index1,index2,pos) ->
				let val1,vars1,_ = eval_rec index1 
				and val2,vars2,_ = eval_rec index2 in
				match val1,val2 with
					| None,_ | _,None -> None,(vars1 @ vars2),pos
					| Some val1,Some val2 ->
						(match index with 
							| MULT_I _	-> Some (val1 * val2),[],pos
							| SUM_I _	-> Some (val1 + val2),[],pos
							| DIV_I _	-> Some (val1 / val2),[],pos
							| MINUS_I _	-> Some (val1 - val2),[],pos
							| POW_I _	-> Some (Tools.pow val1 val2),[],pos
							| MODULO_I _-> Some (val1 mod val2),[],pos
							| _ -> exit 1
						)
		in eval_rec index
	in match dim,r_index with 
		| _,None -> -1, remove_dups r_vars
		| None, Some r_index -> r_index, remove_dups r_vars
		| Some dimension,Some r_index -> 
			if r_index >= dimension || r_index < 0 then
				raise (ExceptionDefn.Semantics_Error (pos,"Array expression out of bounds."))
			else r_index, remove_dups r_vars

(** ?(string * int) list -> ?(int list) -> index_expr list -> int list * string list
 * Returns ( (int) list,[]) if all the index-expression 
 * can be evaluated with the given (var-name-> value) assoc list, or 
 * ([-1],string list) if expression can not be evaluated.
 * string list contain all literals encountered in evaluation, without repeats.
 * If dimensions of compartment is given as 'dims', raise Semantic_error on
 * expressions that are out of bounds.
 *)
let rec partial_eval_indexlist ?(vars=[]) ?(dims=[]) indexlist =
	let r_indexlist,r_vars = 
		let d,d_tl = match dims with
			| [] -> None,[]
			| h :: tl -> Some h,tl
		in
		match indexlist with
		| [] -> ([1],[])
		| [last] -> 
			let index,vars = partial_eval_index ~vars:vars ~dim:d last
			in [index],vars
		| index_term :: index_tl -> 
			let index_hd,vars_hd = partial_eval_index ~vars:vars ~dim:d index_term 
			and index_tl,vars_tl = partial_eval_indexlist ~vars ~dims:d_tl index_tl
			in (index_hd :: index_tl), (vars_hd @ vars_tl)
	in r_indexlist, remove_dups r_vars



(** Returns (A^-1 , b , ?A^t) where A(m*m) and b(m*1) are the matrix in
 * the system A*x=b. If A is not square, then A^t is aplied to A and
 * returned, else third element is None
 *)
let get_matrix_of_expr expr_list var_order =
	(*List of equations as (var,factor list, float)*)
	let equations = List.fold_right (fun expr eq_list ->
		let rec equation expr = (*solving simple equation*)
		match expr with 
		| INT_I (i,pos) -> float(i),[]
		| NAME (s,pos) -> 0.0,[(s, 1.0)]
		(*Do the same with the rest*)
		| MULT_I (index1,index2,pos)
		| SUM_I (index1,index2,pos)
		| DIV_I (index1,index2,pos)
		| MINUS_I (index1,index2,pos)->
		(*| POW (index1,index2,pos)
		| MODULO (index1,index2,pos)*)
			let int1,vars1 = equation index1
			and int2,vars2 = equation index2 in
			let rec iter_vars un_op vars =
				match vars with
				| [] -> []
				| (var,fac):: vars -> (var, un_op fac) :: iter_vars un_op vars
			in
			(match vars1,vars2 with
				| [],[] -> (
					match expr with 
					| MULT_I _	-> int1 *. int2,[]
					| SUM_I _	-> int1 +. int2,[]
					| DIV_I _	-> int1 /. int2,[]
					| MINUS_I _	-> int1 -. int2,[]
					| _ -> exit 1)
				| vars1, [] -> (
					match expr with 
					| MULT_I _	-> int1 *. int2,(iter_vars (fun b -> b *. int2) vars1)
					| SUM_I _	-> int1 +. int2,vars1
					| DIV_I _	-> int1 /. int2,(iter_vars (fun b -> b /. int2) vars1)
					| MINUS_I _	-> int1 -. int2,vars1
					| _ -> exit 1)
				| [], vars2 -> (
					match expr with 
					| MULT_I _	-> int1 *. int2,(iter_vars (fun a -> int1 *. a) vars2)
					| SUM_I _	-> int1 +. int2,vars2
					| DIV_I _	-> (*int1 / int2,iter_vars (fun a b -> a / b) int1 vars2*) 
						raise (ExceptionDefn.Semantics_Error (pos, "Can not solve this equation. Please simplify."))
					| MINUS_I _	-> int1 -. int2,(iter_vars (fun a -> -.a) vars2)
					| _ -> exit 1)
				| vars1, vars2 -> (
					match expr with
					| MULT_I _	-> raise (ExceptionDefn.Semantics_Error (pos, "Only linear equations allowed."))
					| SUM_I _	-> int1 +. int2, vars1 @ vars2
					| DIV_I _	-> raise (ExceptionDefn.Semantics_Error (pos, "Only linear equations allowed."))
					| MINUS_I _	-> int1 -. int2, vars1 @ iter_vars (fun b -> -.b) vars2
					| _ -> exit 1)
				)
		| _ -> exit 1
		in equation expr :: eq_list
	) expr_list [] in
	let vector_b,equations_v =
		let constants,vars = List.split equations 
		in (Array.map (fun a -> -.a) (Array.of_list constants) ),vars
	in
	let matrix_a =
		let row_list = List.fold_right (fun eq_vars row_tl -> 
			let column_list = List.fold_right (fun var_name col_tl ->
				try
					List.assoc var_name eq_vars :: col_tl
				with
					Not_found -> 0. :: col_tl
			) var_order []
			in (Array.of_list column_list) :: row_tl
		) equations_v []
		in Array.of_list row_list
	and matrix_b = Array.make 1 vector_b
	in if Array.length matrix_a <> Array.length matrix_a.(0) then
		let matrix_at = Linear.trans matrix_a
		in Linear.inv (Linear.mul matrix_at matrix_a), matrix_b, Some matrix_at 
	else
		Linear.inv matrix_a,matrix_b,None

(** Solve the system returned by get_matrix_of_expr
 * for a particular cell *)
let solve (a_inv,b0,at_opt) (cell,dims) = 
	let indexlist = cell_to_indexlist cell dims in
	let b_aux = Linear.trans (Array.make 1 (
		Linear.vadd b0.(0) (Array.of_list (List.map (fun i -> float(i) )  indexlist))
	) ) in
	let b = match at_opt with 
		| Some at -> Linear.mul at b_aux 
		| None -> b_aux 
	in Array.to_list (Linear.trans (Linear.mul a_inv b)).(0)
			
(** Validate restriction of variables in a comp_expr
 * and returns true if the compartment meets conditions *)		
let validate cond_opt vars =
	match cond_opt with
		| Some condition ->
			let env,vid_list = List.fold_left (fun (env,v_id_map) (name,value) ->
				let env,v_id = Environment.declare_var_alg (Some (name,Tools.no_pos)) (Some (Mods.Num.I value)) env
				in env,(v_id,value) :: v_id_map
			) (Environment.empty,[]) vars in
			let ( f_val , const, _ , str_val , _ ) = Eval.partial_eval_bool env condition
			and v_of_id = (fun id -> Mods.Num.I (List.assoc id vid_list) )
			and f = (fun _ -> Mods.Num.I 0) 
			in f_val f v_of_id 0. 0 0 0. 0. f
			(*in
			Debug.tag str_val;
			Debug.tag (Tools.string_of_list (fun (a,b) -> a^" -> "^(string_of_int b) ) vars);
			if value then Debug.tag "true" else Debug.tag "false"; value*)
		| None ->
			true
	

		
(** ?((string * int) list) -> index_expr list -> int list -> int list
 * Returns a list with all cells that match with the expression and
 * var values.
*)
let index_expr_to_cells ?(vars_values=[]) indexlist_expr cond dims =
	let size = List.fold_right (fun d t -> d*t) dims 1
	and expr_dim = List.length indexlist_expr in
	try
	let indexlist,var_order = partial_eval_indexlist ~vars:vars_values ~dims:dims indexlist_expr in
	let (some_cells,is_solved,_,_,_) = List.fold_right (fun d (cell_list,solve_expr,indexlist,exprlist,cum) ->
		match indexlist,exprlist with
		| [],[] -> (*Incomplete array expression*)
			let cells = List.fold_right (fun cell cells ->
				let rec iter_d i =
					if i = d then [] 
					else (cell + cum*i ) :: (iter_d (i+1)) 
				in iter_d 0
			) cell_list []
			in (cells,solve_expr,indexlist,exprlist,cum*d)
			
		| (-1 :: index_tl), (expr :: expr_tl) ->
			let cells = List.fold_right (fun cell cells ->
				let rec iter_d i =
					if i = d then [] 
					else (cell + cum*i ) :: (iter_d (i+1)) 
				in (iter_d 0) @ cells
			) cell_list []
			in (cells,false,index_tl,expr_tl,cum*d)
		
		| (index_int :: index_tl), (_ :: expr_tl) ->
			let cells = List.fold_right (fun cell cells ->
				(cell + cum*index_int) :: cells
			) cell_list []
			in (cells,solve_expr,index_tl,expr_tl,cum*d)
		
		| _ , _ -> exit 1

	) dims ([0],true,indexlist,indexlist_expr,1)
	in if is_solved then
		some_cells
	else
		let matrix = get_matrix_of_expr indexlist_expr var_order in 
		List.filter (fun cell -> 
			let solved = solve matrix (cell,dims) in
			(* If all var values are float and eval to cell then true *)
			List.for_all (fun flt -> flt = ceil flt) solved
			&&
			let new_vars_values = List.combine var_order (List.map (fun flt -> int_of_float(flt)) solved) in
			(* Validate restriction of vars *)
			validate cond new_vars_values
			&&
			List.for_all (fun (vname,value) -> 
				try (List.assoc vname vars_values) = value
				with Not_found -> true
			) new_vars_values
			&&
			try let eval_index,_ = (partial_eval_indexlist indexlist_expr ~vars:(new_vars_values @ vars_values) ~dims:dims )
				in eval_index = (cell_to_indexlist cell dims)
			with ExceptionDefn.Semantics_Error (pos,s) -> false
		) some_cells
	with ExceptionDefn.Semantics_Error (pos,s) -> []

(* util? *)
let is_in_cell_list cname cnum cell_list =
	try 
		List.exists (fun cell -> 
			cell = cnum
		) (List.assoc cname cell_list)
	with 
	| Not_found -> false



(** Eval Ast.result_glob.compartments
 * and return a list of (name, (total, dims, vol, pos)) *)
let eval_compartments compartments = 
	Hashtbl.fold (fun cname (index,vol,pos) comparts ->
		let total,dimensions = List.fold_right (fun index_term (tot,dim) ->
			let value,_ = partial_eval_index index_term
			in match value with
				| -1 -> raise (ExceptionDefn.Semantics_Error (pos,"Compartment dimensions must be constant integers."))
				| size -> tot * size, (dim @ [size])
		) index (1,[])
		in (cname, (total, dimensions, vol, pos)) :: comparts
	) compartments []


(** Eval links and return for each statement a function
 * (fun cname cell -> (comp * (cell list) ) list)   that
 * return the list of compartments linked to (cname,cell) *)
let eval_links compartments links =
	Hashtbl.fold (fun lname 
			( ((c1 , pos_c1) , index_expr1, cond1) , ((c2 , pos_c2) , index_expr2, cond2) , is_bidirectional , time , pos ) links ->
		let total1,dims1,_,_ = try List.assoc c1 compartments
			with Not_found -> raise (ExceptionDefn.Semantics_Error (pos_c1,"No compartment with name "^c1^" has been delcared."))
		and total2,dims2,_,_ = try List.assoc c2 compartments 
			with Not_found -> raise (ExceptionDefn.Semantics_Error (pos_c2,"No compartment with name "^c2^" has been delcared."))
		in
		let val1,var_order1 = partial_eval_indexlist ~dims:dims1 index_expr1
		and val2,var_order2 = partial_eval_indexlist ~dims:dims2 index_expr2 in
		let func_get_links = (
		match var_order1,var_order2 with
			| [],[] ->
				let cell_list1 = index_expr_to_cells index_expr1 cond1 dims1
				and cell_list2 = index_expr_to_cells index_expr2 cond2 dims2 in
				(fun cname cell ->
					let l1 = if c1 = cname && List.exists (fun c -> c = cell) cell_list1 then
						[c2,cell_list2]
						else []
					and l2 = if is_bidirectional && c2 = cname && List.exists (fun c -> c = cell) cell_list2 then
						[c1,cell_list1]
						else []
					in l1 @ l2
				)
			| [],_ | _,[] -> 
				let cell_list1 = index_expr_to_cells index_expr1 cond1 dims1
				and cell_list2 = index_expr_to_cells index_expr2 cond2 dims2 in
				(fun cname cell ->
					let l1 = if c1 = cname && List.exists (fun c -> c = cell) cell_list1 then
						[c2,cell_list2]
					else []
					and l2 = if is_bidirectional && c2 = cname && List.exists (fun c -> c = cell) cell_list2 then
						[c1,cell_list1]
					else []
					in l1 @ l2
				)
			| var_order1,var_order2 -> 
				let m1 = get_matrix_of_expr index_expr1 var_order1
				and m2 = get_matrix_of_expr index_expr2 var_order2 in
				let cell_list1 = index_expr_to_cells index_expr1 cond1 dims1
				and cell_list2 = index_expr_to_cells index_expr2 cond2 dims2 in
				(fun cname cell ->
				let l1 = if c1 = cname && List.exists (fun c -> c = cell) cell_list1 then
					let solved = solve m1 (cell,dims1) in
					(* If all var values are float and eval to cell then true *)
					if List.for_all (fun flt -> flt = ceil flt) solved then
						let vars_values = List.combine var_order1 (List.map (fun flt -> int_of_float(flt)) solved) in
						[c2,index_expr_to_cells ~vars_values:vars_values index_expr2 cond2 dims2]
					else []
				else []
				and l2 = if is_bidirectional && c2 = cname && List.exists (fun c -> c = cell) cell_list2 then
					let solved = solve m2 (cell,dims2) in
					(* If all var values are float and eval to cell then true *)
					if List.for_all (fun flt -> flt = ceil flt) solved then
						let vars_values = List.combine var_order2 (List.map (fun flt -> int_of_float(flt)) solved) in
						[c1,index_expr_to_cells ~vars_values:vars_values index_expr1 cond1 dims1]
					else []
				else []
				in l1 @ l2
				)
			)
		in Hashtbl.add links lname (func_get_links,time);
		links
	) links (Hashtbl.create (Hashtbl.length links))

(** Eval use expressions and returns (cell list) array
 * ordered by statements *)
let eval_use compartments use_expressions = 
	Array.of_list 
		(List.fold_right (fun use_expr_opt cells_list ->
			match use_expr_opt with
			| None -> [] :: cells_list
			| Some use_expr -> (List.fold_right (fun ((cname,pos),indexlist_expr,cond_opt) cells ->
					let _,dims,_,_ = 
						try List.assoc cname compartments 
						with Not_found -> raise (ExceptionDefn.Semantics_Error (pos,"No compartment with name "^cname^" has been delcared."))
					in
					(cname, (index_expr_to_cells indexlist_expr cond_opt dims)) :: cells
				) use_expr []) :: cells_list
		) use_expressions []
	)

let is_in_use_expr cname cnum use_id use_cells =
	try 
		let cells = use_cells.(use_id) in
		match cells with
			| [] -> true
			| cells_list ->
				List.exists (fun cell -> cell = cnum) (List.assoc cname cells_list)
	with 
	| Not_found -> false
	| Invalid_argument _ -> true


(** Eval result obtained from parse files and returns a list
 * of Ast.compil for being reevaluated in each compartment. *)
let initialize_glob result_glob = 
	let total_comparts = eval_compartments result_glob.Ast.compartments in
	let total_cells = List.fold_right (fun (_,(l,_,_,_)) n -> n + l) total_comparts 0 in 
	
	let result_links = eval_links total_comparts result_glob.Ast.links in
	
	let use_cells = eval_use total_comparts result_glob.Ast.use_expressions in
	
	let is_in_use_expr cname cnum use_id = is_in_use_expr cname cnum use_id use_cells in
	
	let is_in_use_expr cname cnum use_id =
		try 
			let cells = use_cells.(use_id) in
			match cells with
				| [] -> true
				| cells_list ->
					List.exists (fun cell -> cell = cnum) (List.assoc cname cells_list)
		with 
		| Not_found -> false
		| Invalid_argument _ -> true
	in
	
	let result_list = List.fold_right (fun compartment compils ->
		let rec all_cells (cname,(length,dims,vol,pos)) cnum = 
			if cnum = length then
				[]
			else
			((cname,cnum) , 
			{Ast.volume = vol; 
			Ast.dims = dims;
			Ast.signatures = result_glob.Ast.signatures_g;
			Ast.observables = result_glob.Ast.observables_g;
			Ast.configurations = result_glob.Ast.configurations_g;
			Ast.tokens = result_glob.Ast.tokens_g;
			
			(*Ast.volumes = (
				List.fold_left (fun v_list (vol,use_id) ->
					if is_in_use_expr cname cnum use_id then
						vol::vol_list
					else vol_list
				) [] result_glob.Ast.volumes_g
			);*)
			
			Ast.variables = (
				List.fold_left (fun v_list (var,use_id) ->
					if is_in_use_expr cname cnum use_id then
						var::v_list
					else v_list
				) [] result_glob.Ast.variables_g
			);
			
			Ast.perturbations = (
				List.fold_left (fun p_list (pert,use_id) ->
					if is_in_use_expr cname cnum use_id then
						pert::p_list
					else p_list
				) [] result_glob.Ast.perturbations_g
			);
		
			Ast.init = [];
			
			Ast.rules = ( 
				let local_rules = (* add only global rules or local to compartment *)
					List.fold_left (fun r_list (label,rule) -> 
						if is_in_use_expr cname cnum rule.Ast.use_id then
							(* Ajuste por volumen *)
							let rule' = 
								if rule.Ast.fixed = false then
									let arity = arity_of_astmix rule.Ast.lhs in
									(*match label.Ast.lbl_nme with | None -> () | Some (s,_) ->
										if arity != 1 then Debug.tag("rule: "^s^"\tarity:"^(string_of_int arity)^"\tvol: "^(string_of_float (match vol with | Ast.FLOAT (f,_) -> f | _ -> 0.0))));**)
									{rule with 
										Ast.k_def = Ast.MULT 
											(Ast.POW ( vol ,Ast.FLOAT (
												float_of_int (1-arity),Tools.no_pos),
												Tools.no_pos ),
											 rule.Ast.k_def, Tools.no_pos
											)
									}
								else
									rule
							in (label,rule') :: r_list
						else r_list
					) [] result_glob.Ast.rules_g
				in let local_transports = 
					let fresh_id = ref 0 in
					List.fold_left (fun trans_list ( (lname,_), mixt, trans_rate, joined, pos ) -> (*iter transports*)
						(*TODO match fixed rates*)
						let scaled_trans_rate = trans_rate in (*Ast.DIV(trans_rate,vol,Tools.no_pos) in*)
						List.fold_left (fun t_link_list (func_get_links,travel) -> (*iter links 'lname'*)
							let dest_cells = func_get_links cname cnum in
							let length_dest_ast = Ast.FLOAT (float_of_int (cell_list_length dest_cells),Tools.no_pos) in
							(List.map (fun (cname2,cells) -> (*iter destination cells*)
								List.fold_left (fun t_rules cell ->
								if cname = cname2 && cnum = cell then
									(Printf.printf "*** (%s) line %d, char %d: declaration implies 'self to self' transport.(aborting)\n" 
										(fn pos) (ln pos) (cn pos); t_rules)
								else
									match mixt with
									| Ast.COMMA(agent,mixture) -> 
										(*Debug.tag("Creating diffusion rule: "^cname^"["^(string_of_int cnum)^"] -> "^cname2^"["^(string_of_int cell)^"]");*)	
										(*Debug.tag ("length_dest"^ (string_of_int (cell_list_length dest_cells)));*)
										fresh_id := !fresh_id+1;
										({
										Ast.lbl_nme = Some ("TRANSPORT-"^lname^" #"^(string_of_int !fresh_id),Tools.no_pos);
										Ast.lbl_ref = None;
										},{
										Ast.rule_pos = pos;
										Ast.lhs = mixt;
										Ast.rm_token = [];
										Ast.arrow = Ast.RAR Tools.no_pos;
										Ast.rhs = Ast.EMPTY_MIX;
										Ast.add_token = [];
										(*TODO add surface expr*)
										Ast.k_def = Ast.DIV(scaled_trans_rate, length_dest_ast , Tools.no_pos);
										Ast.k_un = None;
										Ast.k_op = None;
										Ast.transport_to = Some ((cname2,cell),travel,joined);
										Ast.use_id = -1;
										Ast.fixed = true;
										}) :: t_rules
									| _ -> Debug.tag "Transport Rule with no Agent\n" ;exit 1
								) [] cells
							) dest_cells ) @ t_link_list 
						) [] (Hashtbl.find_all result_links lname)
						@ trans_list
					) [] result_glob.Ast.transports
				in List.concat (local_transports) @ local_rules
			);
		
			}) :: all_cells compartment (cnum+1)
		in compils @ (all_cells compartment 0)
	) total_comparts []
	in (*result_list*)
	
	let _,first_res_ast = (List.hd result_list) in
	let temp_env = Eval.environment_of_result first_res_ast in
	let (temp_env, kappa_vars, alg_vars) = Eval.variables_of_result temp_env first_res_ast in
	
	List.fold_right ( fun (vol,init_t,pos,use_id) result_list ->
		match init_t with
		| Ast.INIT_MIX (expr,mixt) -> 
			let cells_len = 
				let len = List.fold_right (fun (_,cells) n -> n + List.length cells ) use_cells.(use_id) 0 in
				match len with
					| 0 -> total_cells
					| n -> n
			in
			let (v, is_const, opt_v, dep, lbl) = Eval.partial_eval_alg temp_env expr in
			let value = 
				match opt_v with
					| Some v -> Num.int_of_num v
					| None -> raise (ExceptionDefn.Semantics_Error (pos, Printf.sprintf "%s is not a constant, cannot initialize graph." lbl))
  			in
  			let rest = value mod cells_len in
			let result_list,_ = List.fold_right (fun ((cname,cnum),result_ast) (new_result_list,distr) ->
				if is_in_use_expr cname cnum use_id then
					if cells_len > 1 then
						let new_int_expr = Ast.INT (value / cells_len + (if List.hd distr then 1 else 0) , pos) in
						let new_result = {result_ast with Ast.init = (vol,Ast.INIT_MIX (new_int_expr,mixt), pos) :: result_ast.Ast.init}
						in (((cname,cnum), new_result ) :: new_result_list ), List.tl distr
					else
						let new_result = {result_ast with Ast.init = (vol,Ast.INIT_MIX (expr,mixt),pos) :: result_ast.Ast.init}
						in (((cname,cnum), new_result ) :: new_result_list) , distr 
				else
					(((cname,cnum),result_ast ) :: new_result_list) , distr
			) result_list ([],Spatial_util.distribute cells_len rest)
			in result_list
		| Ast.INIT_TOK (expr,tok) -> 
			result_list
	) result_glob.Ast.init_g result_list
	

	
	
	
