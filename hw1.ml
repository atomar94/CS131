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

(* returns a list that is a mathematical set, i.e. no duplicates *)
let rec make_set a =
    match a with
      [] -> []
    | hd::tl -> if (contains hd tl) then
                    make_set tl
                else
                    hd::make_set tl;;




(*
 * HW FUNCTION - subset
 *
 * remove the first element of a from both a and b, then recurse.
 * if that element does not exist in b then a is not a subset.
 * *)
let rec subset a b =
    let a = make_set a in
    let b = make_set b in
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
    let a = make_set a in
    let b = make_set b in
 
    match a with
      []     -> if b=[] then true else false
    | hd::tl -> if contains hd b
                then let bb = remove hd b in
                  equal_sets tl bb
                else false;;


(*
 * HW FUNCTION - set_union
 *
 * if an element is in one of the sets add it to the union.
 * if it is in both, only add it once.
 *)
let rec set_union a b =
    let a = make_set a in
    let b = make_set b in
    let big = a @ b in

    match big with
      []     -> []
    | hd::tl -> let aa = remove hd a in
                let bb = remove hd b in

                if (contains hd a) && (contains hd b) then
                    hd::set_union aa bb
                else begin
                    if contains hd a
                    then
                        hd::set_union aa b
                    else begin
                        if contains hd b
                        then
                            hd::set_union a bb
                        else (*logically this case will never be true, but there
                               is some syntax bug in ocaml that forces me to have
                               this else clause.
                               see http://stackoverflow.com/a/22052996 *)
                            []
                    end
                end;;


(*
 * HW FUNCTION - set_intersection
 *
 * if an element exists in both sets then add it to our
 * returned list, else dont add anything. recurse.
 * *)
let rec set_intersection a b =
    let a = make_set a in
    let b = make_set b in
 
    let big = a @ b in

    match big with
      []     -> []
    | hd::tl -> if (contains hd a) && (contains hd b)
                then begin
                    let aa = remove hd a in
                    let bb = remove hd b in
                        hd::set_intersection aa bb
                end else begin
                    let aa = remove hd a in (*if hd isnt in a then nothing happens*)
                    let bb = remove hd b in
                        set_intersection aa bb
                end;;

(*
 * HW FUNCTION - set_diff
 *
 * remove all elements in b from a. if the element
 * doesnt exist in a then a remains unmodified.
 * *)
let rec set_diff a b =
    let a = make_set a in
    let b = make_set b in
    match b with
      []     -> a
    | hd::tl -> let aa = remove hd a in
                set_diff aa tl;;

(*
 * HW FUNCTION
 *
 * iteratively finds the fixed point according to the wikipedia article
 * *)
let rec computed_fixed_point eq f x =
    let result = f x in
    if  (eq) result x then x else (computed_fixed_point eq f result);;    


(*
 * HW FUNCTION
 *
 * if the case is true then add the value to list and recurse,
 * else return empty list.
 * *)
let rec while_away s p x = 
    let condition = p x in
    match condition with
      true -> x::while_away s p (s x)
    | false -> [] (* false so done *)







