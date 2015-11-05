
open Mods

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
	

let cell_list_length cells_list =
	List.fold_left (fun n (_,cells) -> n + (List.length cells) ) 0 cells_list


let string_of_comp ?(dims_opt) (cname,cnum) =
	match dims_opt with
		| None ->
			cname ^ "{" ^ (string_of_int cnum) ^ "}"
		| Some [] ->
			cname
		| Some dims ->
			let indexlist = cell_to_indexlist cnum dims 
			in cname ^ "["^(String.concat "][" (List.rev_map string_of_int (List.rev indexlist)))^"]"
			
let is_main () = (Mpi.comm_rank Mpi.comm_world) = 0

let identity a = a

let rec sum_floatlist l = match l with
	| [] -> 0.0
	| err :: tl -> err +. sum_floatlist tl
			
let time_table = ref (Hashtbl.create 10)

(**	Starts a timer at this point of execution with a tag*)
let start_timer tag =
	Hashtbl.add !time_table tag (Unix.gettimeofday())

(**	Stops the timer 'tag' and saves in time table*)
let stop_timer tag =
	let start = Hashtbl.find !time_table tag in
	let interval = (Unix.gettimeofday ()) -. start in
	Hashtbl.replace !time_table tag interval;
	interval

(** Stops the timer 'tag' and adds it in time table*)
let stop_sum_timer tag =
	let start = Hashtbl.find !time_table tag in
	let interval = (Unix.gettimeofday ()) -. start in
	Hashtbl.remove !time_table tag;
	let sum = try Hashtbl.find !time_table tag with Not_found -> 0.0 in
	Hashtbl.replace !time_table tag (interval+.sum);
	interval
	
(**	Show all timers*)
let show_timer () =
	let curr_lab = ref "" in
	Hashtbl.iter (fun label time ->
		if label = !curr_lab then
			Printf.printf "\t%f" time
		else
			(curr_lab := label; Printf.printf "\n%s\t%f" label time)
	)
	!time_table;
	Debug.tag "\n"
	
let print_sync_info counter old_totals totals activity comps =
	match counter.Counter.progress_step with
		| None -> ()
		| Some n -> 
			Debug.tag "_______________________________________________";
			Debug.tag (Printf.sprintf "Synchronization %d-%d\n\tSimulated-Time:\t%f\tTotal Events:\t%d\t\tActivity:%f\n\t---- Stats for this period ----\n\tReaction Ev: %d \tDiffusion Ev: %d\n\tAverage Events per Sync. per Compart. : %.1f\n\tTotal-Sync-Error:\t%f" 			
				((Counter.get_sync_count counter) - n + 1) (Counter.get_sync_count counter)
				(Counter.time counter) (totals.(0)) activity
				(totals.(1) - old_totals.(1)) (totals.(2) - old_totals.(2))
				(float(totals.(1)+totals.(2) - old_totals.(1)-old_totals.(2)) /. float(n) /. float(comps) )
				(sum_floatlist !Quality.syncErrors))


let rec distribute ?(inv=false) c n =
	if n > c/2 then distribute ~inv:true c (c-n) 
	else
		let arr = Array.init c (fun i -> (i,inv)) in
		let rec iter index_list arr k =
			match k with
				| 0 -> let _,dist = 
					List.split (List.sort (fun (a1,a2) (b1,b2) -> compare a1 b1) (index_list @ (Array.to_list arr)))
					in (*Array.of_list*) dist
				| k -> let i = Random.int (c-n+k) in
					(*Printf.printf "i %d \n" i;*) 
					arr.(i) <- (let ind,_ = arr.(i) in (ind,not inv));
					let elem = arr.(i) in
					iter (elem::index_list) (Array.append (Array.sub arr 0 i) (Array.sub arr (i+1) ((Array.length arr)-i-1)) ) (k-1)
		in iter [] arr n






