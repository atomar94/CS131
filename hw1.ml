(* CS 131 HW 1
 * Alex Omar - 004 315 572
 *)

(* Returns a list with one element matching e removed *)
let rec remove e l =
    match l with
      [] -> l
    | hd::tl -> if hd=e
                then tl
                else hd :: (remove e tl);;

(* True if a list l contains an element e *)
let rec contains e l =
    match l with
      [] -> false
    | hd::tl -> if hd=e
                then true
                else (contains e tl);;


let rec subset a b = 
    match a with
      [] -> true
    | hd::tl -> if (contains hd b)
                then let bb = (remove hd b) in
                    subset tl bb
                else false;;











