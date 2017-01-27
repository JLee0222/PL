let rec range n =
	match n with
	| 0 -> []
	| _ -> (range (n-1)) @ [n-1];;

let rec reverse xs =
	match xs with
	| [] -> []
	| hd :: tl -> (reverse (tail)) @ [head];;

let rec mem x xs =
	match xs with
	| [] -> false
	| y :: ys -> (x = y) || (mem x ys);;

let rec intersection xs ys =
	match xs with
	| [] -> []
	| z :: zs ->
	 (match mem z ys with
	  | true -> z :: intersection zs ys
	  | false -> intersection zs ys);;

let rec sum xs ys =
	match xs with
	| [] ->
	 (match ys with
	  | [] -> []
	  | w :: ws -> (1, w) :: (sum xs ws))
	| z :: zs -> (0, z) :: (sum zs ys);;

let rec forall test xs =
	match xs with
	| [] -> failwith "NO"
	| [x] -> test x
	| z :: zs -> ((test z) && (forall test zs));;