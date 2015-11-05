open Mods
open Tools
open Mpi
open Random_tree


let world_size = comm_size comm_world
let myrank = comm_rank comm_world


let check_mpi_processes value succes_string =
	let error_count = 
		allreduce_int value Int_sum comm_world
	in
	if error_count > 0 && value = 0 then 
		(if myrank = 0 then
			Debug.tag ((string_of_int error_count) ^ "mpi-processes raise exceptions.");
		exit 1)
	else
		if myrank = 0 then Debug.tag succes_string
	
	

type sync_mode =
	| ALLGATHER
	| GATHER
	| SEND

let get_syncmode = 
	match !Parameter.syncMode with
		| None -> ALLGATHER
		| Some str -> 
			if String.compare str "allgather" = 0 then
				ALLGATHER
			else if String.compare str "gather" = 0 then
				GATHER
			else if String.compare str "send" = 0 then
				SEND
			else 
				raise (Mpi.Error "Not a valid sync-mode")
				



let synch_gather comp_map comp_id transports = 
	let t_data = Hashtbl.fold (fun comp_id2 id t_m ->
		if comp_id2 != comp_id then
			let data = 
				try None,Some (Hashtbl.find transports comp_id2)
				with Not_found -> None,None
			in
				ignore (gather data id comm_world);
				t_m
		else
			gather (None,None) id comm_world
	) comp_map [|None,None|] in
	Hashtbl.clear transports;
	
	let transport_messages = Array.map (fun  t_msg ->
		match t_msg with
				| (Some exit_msg),_ -> Debug.tag exit_msg; exit 1
				| None, t -> t
				(*| None, None -> None*)
	) t_data in
	transport_messages



let synch_allgather comp_map comp_id transports = 
	let t_data = allgather (None, transports) comm_world in
	Hashtbl.clear transports;
	
	let transport_messages = 
		Array.map (fun (error_msg,transport_recv) ->
			match error_msg with
				| None -> (
					try Some (Hashtbl.find transport_recv comp_id)
					with Not_found -> None )
				| Some msg ->
					exit 1
		) t_data
	in transport_messages



let transport_synchronize comp_map comp_id transports = 
	match get_syncmode with
		| ALLGATHER -> synch_allgather comp_map comp_id transports
		| GATHER -> synch_gather comp_map comp_id transports
		| _ -> exit 1

let local_counter_array counter =
	[| counter.Counter.events ;
		counter.Counter.reaction_ev ;
		counter.Counter.diffusion_ev ;
		counter.Counter.null_events ; 
		counter.Counter.cons_null_events ;
		counter.Counter.perturbation_events ; 
		counter.Counter.null_action |]

let total_counter_synchronize counter =
	let data_array = local_counter_array counter in
	allreduce_int_array data_array counter.Counter.total_events Mpi.Int_sum comm_world;
	counter.Counter.total_events
	

let allreduce_float_array arr =
	let result = Array.make (Array.length arr) 0. in
	Mpi.allreduce_float_array arr result Mpi.Float_sum comm_world;
	result


let propagate_error ex_msg =
	match get_syncmode with
		| ALLGATHER -> ignore (allgather (ex_msg,None) comm_world)
		| GATHER -> 
			for  i = 0 to world_size - 1 do
				ignore (gather (ex_msg,None) i comm_world)
			done
		| _ -> exit 1


	
let update_state state env counter = 
 	let act_tree = (*initializing activity tree*)
 		(*Hashtbl.fold (fun id rule act_tree ->
 			(*rule could be a perturbation*)
 			if not (Environment.is_rule id env) then act_tree
 			else
 				let a2,a1 = State.eval_activity rule state counter env in
 				let alpha_rule = Num.float_of_num (Num.add a1 a2) in
 				(Random_tree.add id alpha_rule act_tree ; act_tree)
 		) state.State.rules *)state.State.activity_tree
 	in
 	if !Parameter.debugModeOn then Debug.tag "\t * Computing influence map...";
 	let im = State.build_influence_map state.State.rules state.State.kappa_variables env 
 	in
 	({state with State.activity_tree = act_tree; influence_map = im}, env)



