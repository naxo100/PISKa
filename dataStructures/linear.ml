module Linear = struct
let len = Array.length and aini = Array.init

let print m mat = let i = len mat and j = len mat.(0) in
print_string m; print_newline ();
  for i = 0 to i-1 do for j = 0 to j-1 do print_float mat.(i).(j); print_char ' ' done;
  print_newline () done

(* OCaml code to invert a matrix *)
exception Singular
let inv mat =
let l = len mat in
let am = Array.mapi (fun m row -> (Array.append row
      (aini l (fun n -> if m=n then 1. else 0.)))) mat in
for i = 0 to l-1 do (let im = ref 0 and mv = ref (abs_float am.(i).(i)) in
   for j = i+1 to l-1 do (let ae = abs_float am.(j).(i) in
       if (!mv < ae) then (mv := ae; im := j)) done;
   if !mv = 0. then raise Singular;
   if !im > i then (for n = i to (2*l - 1) do
      (let s = am.(i).(n) in am.(i).(n) <- am.(!im).(n); am.(!im).(n) <- s) done);
   let r = 1. /. am.(i).(i) in
   for j = i to 2*l - 1 do (am.(i).(j) <- r *. am.(i).(j)) done;
   for k = i+1 to l-1 do (let f = am.(k).(i) in
      for j = i+1 to 2*l - 1 do (am.(k).(j) <- am.(k).(j) -. f *. am.(i).(j))
      done); done) done;
for i = 0 to l-1 do (for j = i+1 to l-1 do (let p = am.(i).(j) in
      for k = i+1 to 2*l - 1 do
         (am.(i).(k) <- am.(i).(k) -. am.(j).(k) *. p) done) done) done;
Array.map (fun row -> Array.sub row l l) am

(* Matrix Multiply *)
exception LopSided
let mul a b =
   let l1 = len a and l2 = len a.(0)
   and l3 = len b and l4 = len b.(0) in
   if l2 <> l3 then raise LopSided;
   aini l1 (fun i -> aini l4 (fun k ->
     let c = ref 0. in for j = 0 to l2-1 do
        c := !c +. a.(i).(j) *. b.(j).(k) done; !c))

let determ a = (let s = len a in let w = Array.make (s * s) 0. in
  for i = 0 to s-1 do (for j = 0 to s-1 do (w.(s*i + j) <- a.(i).(j)) done) done;
  let rec det b k = (if k = 1 then w.(b)
    else (let rec piv j = if j=k then 0.
     else (if w.(b + s*j) = 0. then piv (j+1)
       else (if j <> 0 then (let q = b + j*s in
         for n = 0 to k-1 do (let t = w.(q + n) in
             w.(q + n) <- w.(b+n); w.(b+n) <- -. t) done);
         (let t = 1. /. w.(b) in for i = 1 to k-1 do (
            let q = b + i*s in let a = t *. w.(q) in
               for r=1 to k-1 do (w.(q+r) <- w.(q+r) -. a *. w.(b+r)) done) done);
          w.(b) *. (det (b+s+1) (k-1)))) in piv 0)) in det 0 s)

exception Ragged
let trans a = let m = len a and n = len a.(0) in
  for i = 1 to m-1 do if len a.(i) <> n then raise Ragged done;
   aini n (fun i -> aini m (fun j -> a.(j).(i)))

let ip a b = let n = len a and s = ref 0. in
   if n <> len b then raise Ragged;
   for i = 0 to n-1 do s := !s +. a.(i) *. b.(i) done; !s

let mtv a b = let l1 = len a.(0) and l2 = len b in
   if l1 <> l2 then raise LopSided;
   aini (len a) (fun j -> ip a.(j) b)

let vneg a = aini (len a) (fun j -> -. a.(j))

let sm sc a = aini (len a) (fun j -> sc *. a.(j))

let vadd a b = let l = (len a) in assert (l = (len b)); aini l (fun j -> a.(j) +. b.(j))

let iden n = (Array.init n (fun i -> Array.init n (fun j -> if i=j then 1. else 0.)))

let gs dp bas = let n = len bas and m = len bas.(0) in
  let nb = Array.make n [||] in for i = 0 to n-1 do nb.(i) <- Array.copy bas.(i);
  for k = 0 to i-1 do let ip = dp nb.(i) nb.(k) in for j = 0 to m-1 do
      nb.(i).(j) <- nb.(i).(j) -. ip *. nb.(k).(j) done done;
    nb.(i) <- sm (1. /. (sqrt (dp nb.(i) nb.(i)))) nb.(i) done; nb

let eigen m = let n = len m in let m2 = aini n (fun j -> Array.copy m.(j)) in
(let id = iden n in (let mOd = ref 0. and ip = ref (-1, -1) in
  while mOd := 0.; ip := (-1, -1);
  for i=0 to n-1 do for j=i+1 to n-1 do let q = (abs_float m2.(i).(j)) in
     if q > !mOd then (mOd := q; ip := (i, j)) done done;
     !mOd > 0.00000000001 do let (i, j) = !ip in
     let th = (0.5 *. (atan (2. *. (m2.(i).(j) /. (m2.(i).(i) -. m2.(j).(j)))))) in
     (let c = cos th and s = sin th in let twst m = (let tmp = vadd (sm c m.(i)) (sm s m.(j)) in
       m.(j) <- vadd (sm (-. s) m.(i)) (sm c m.(j)); m.(i) <- tmp) in
    for k=0 to n-1 do (let tmp = c *. m2.(k).(i) +. s *. m2.(k).(j) in
         m2.(k).(j) <- (-. s) *. m2.(k).(i) +. c *.  m2.(k).(j);  m2.(k).(i) <- tmp) done;
    (twst m2; twst id)) done;
     aini n (fun j -> m2.(j).(j), id.(j))))

end;;
