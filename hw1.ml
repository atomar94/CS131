(* CS 131 HW 1
 * Alex Omar - 004 315 572
 *)

(* Returns a list with one element matching e removed *)
let rec remove e l =
    match l with
      []     -> l
    | hd::tl -> if hd=e
                then tl
                else hd :: (remove e tl);;

(* True if a list l contains an element e *)
let rec contains e l =
    match l with
      []     -> false
    | hd::tl -> if hd=e
                then true
                else (contains e tl);;


(*
 * HW FUNCTION - subset
 *
 * remove the first element of a from both a and b, then recurse.
 * if that element does not exist in b then a is not a subset.
 * *)
let rec subset a b = 
    match a with
      []     -> true
    | hd::tl -> if (contains hd b)
                then let bb = (remove hd b) in
                    subset tl bb
                else false;;
(*
 * HW FUNCTION - equal_sets
 *
 * remove the first element of a from b, then recurse again
 * but pass in the two modified lists.
 * *)
let rec equal_sets a b =
    match a with
      []     -> if b=[] then true else false
    | hd::tl -> if contains hd b
                then let bb = remove hd b in
                  equal_sets tl bb
                else false;;


(*
 * HW FUNCTION - set_union
 *
 * if an element exists in both sets then add it to our
 * returned list, else dont add anything. recurse.
 * *)
let rec set_union a b =
    let big = a @ b in

    match big with
      []     -> []
    | hd::tl -> if (contains hd a) && (contains hd b)
                then begin
                    let aa = remove hd a in
                    let bb = remove hd b in
                        hd::set_union aa bb
                end else begin
                    let aa = remove hd a in (*if hd isnt in a then nothing happens*)
                    let bb = remove hd b in
                        set_union aa bb
                end;;





