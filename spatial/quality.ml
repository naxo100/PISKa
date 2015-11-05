open Random_tree

let (syncErrors: float list ref) = ref []

let activity_list state counter env =
	let total_activity = Random_tree.total state.State.activity_tree in
	Hashtbl.fold
	(fun i r l ->
		if Environment.is_rule i env then
		let a2,a1  = State.eval_activity r state counter env in
			l @ [Random_tree.find i state.State.activity_tree]
		else l
	) state.State.rules [total_activity]


let activity_list2 state counter env =
	let total_activity = Random_tree.total state.State.activity_tree in
	Hashtbl.fold
	(fun i r l ->
		let nme =
			try "'" ^ ((Environment.rule_of_num i env) ^ "'")
			with | Not_found -> ""
		in
		let a2,a1  = State.eval_activity r state counter env in
		if Environment.is_rule i env then
			l @ [nme,(Random_tree.find i state.State.activity_tree)]
		else l
	) state.State.rules [("total",total_activity)]

let new_dt dT' dA old_A =
	if dT' = infinity && dA >= 0. then None
	else Some (old_A *. dT' /. (old_A +. dA))


let rule_names state counter env =
	Hashtbl.fold
	(fun i r l ->
		let nme =
			try "'" ^ ((Environment.rule_of_num i env) ^ "'")
			with | Not_found -> ""
		in
		if Environment.is_rule i env then
			l @ [nme]
		else l
	) state.State.rules ["total"]
	
	
let average_delay transports =
	let sum,count = 
	Array.fold_left (fun (sum,count) transport_compart ->
		match transport_compart with
			| None -> sum,count
			| Some transport_map ->
				Hashtbl.fold (fun _ (late_count,late_sum,_) avrg ->
					(sum +. late_sum),count + late_count
				) transport_map (sum,count)
	) (0.0,0) transports
	in if count > 0 then sum /. (float_of_int count) else 0.0
	
let transport_error pre_act post_act average_delay =
	let pre_total,post_total = (List.hd pre_act),(List.hd post_act) in
	if pre_total = post_total then
		0.0
	else
		if pre_total = 0.0 then
			(post_total +. 1.0) *. average_delay
		else
			let diff = ((abs_float (post_total -. pre_total)) +. 1.0) /. (pre_total +. 1.0)
			and n_rules = (List.length pre_act) - 1 in
			let p = List.fold_left2 (fun value pre post ->
				value +. ((abs_float (post -. pre)) +. 1.0) /. (pre +. 1.0) /. (float_of_int n_rules)
			) diff (List.tl pre_act) (List.tl post_act)
			in p *. average_delay
	

	
	
	
