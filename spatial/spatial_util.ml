
(** 'a list -> a' list
 * Remove duplicates from a list *)
let rec remove_dups lst = 
	match lst with
	| [] -> []
	| h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))
	
			
(** Returns (int) the value of cell in a matrix 
 * converting a multidimensional matrix to a simple vector *)
let rec indexlist_to_cell ?(cum=1) indexlist dims =
	match indexlist,dims with
		| [],[] -> 0
		| i::i_tl , [] -> i
		| [] , d :: d_tl -> exit 1
		| i::i_tl , d :: d_tl -> cum*i + (indexlist_to_cell ~cum:(cum*d) i_tl d_tl )

(**Returns (int list) the index list corresponding to the cell
 * in vector to matrix.*)
let cell_to_indexlist cell dims =
	let total = List.fold_right (fun d t -> d*t) dims 1 in 
	let rec to_indexlist cell dims cum = 
		match dims with
		| [] -> [1]
		| [ d ] -> [cell]
		| d :: d_tl ->
			let new_cum,new_cell = (cum / d),cell*d/cum
			in new_cell :: (to_indexlist (cell-new_cell*new_cum) d_tl new_cum)
	in List.rev (to_indexlist cell (List.rev dims) total)
	


let string_of_comp ?(dims_opt) (cname,cnum) =
	match dims_opt with
		| None ->
			cname ^ "{" ^ (string_of_int cnum) ^ "}"
		| Some dims ->
			let indexlist = cell_to_indexlist cnum dims 
			in cname ^ "["^(String.concat "][" (List.rev_map string_of_int (List.rev indexlist)))^"]"
			
let is_main () = (Mpi.comm_rank Mpi.comm_world) = 0
			

