(* hw2.ml *)

open List;;

type ('nonterminal, 'terminal) symbol =
      | N of 'nonterminal
      | T of 'terminal;;


(* given a rule of form (nonterminal, rule list[]) and (element_nonterminal, rule_list[])  
 * this will return true if they are the same type of nonterminal.
 * *)
let convert_grammar_filter (match_nonterm, _) elem =
    match elem with
      (e_nonterm, _) ->  if match_nonterm=e_nonterm then true else false
    | _ -> false;;


let convert_grammar_filter_negate (match_nonterm, _) elem =
    match elem with
      (e_nonterm, _) ->  if match_nonterm=e_nonterm then false else true
    | _ -> true;;

(*
 * Alright, so
 *
 * 2 lists. "notyet" holds the rules in our grammar that havent been properly grouped (converted to hw2 format) yet
 * grouped holds the rules that have been grouped according to hw2 specs.
 *
 * get the first rule of notyet, find all the rules in notyet that have that same nonterminal symbol, and create a big
 * list of the rules.
 *
 * take that big list of rules and add it to grouped. remove everything that went into the big list of rules from notyet.
 *
 * recurse.
 *)
let rec group_rules notyet grouped =
    match notyet with
     (nonterm, rules)::tl -> let grammar_filter = (convert_grammar_filter (nonterm, rules)) in
                let matching_nonterms = (List.filter grammar_filter notyet) in
                let (_, rule_list) = List.split matching_nonterms in
                let grammar_filter_neg = (convert_grammar_filter_negate (nonterm, rules)) in
                group_rules (List.filter grammar_filter_neg notyet) (grouped @ [(nonterm, rule_list)])
    | [] -> grouped;;


(* -- Production Function --
 *
 * Given a Grammar of the Second Kind (hw 2 grammar) return the alternative list 
 * i.e. a list of possible rules to apply to that nonterm 
 * *)
let rec get_alt_list grlist nonterm =
    match grlist with
      (nt, rules)::tl -> if nt=nonterm then rules else get_alt_list tl nonterm
    | [] -> [];;


let convert_grammar gram1 =
    match gram1 with
    (start, gr) -> (start, get_alt_list (group_rules gr []));;


(* Given a rule (i.e. one line in the alt list) check to see if it matches with frag.
 * if not: return none
 * if it does: return what acceptor returns
 * if frag has nonterminals: we need to find a rule that applies (using find_rule) *)
let rec match_rule pf rule a d f =
    match rule with
    | [] -> a d f (* our rule is done matching! return what acceptor returns. *)
    | r_hd::r_tl -> ( match r_hd with (* frag only has terminals so we have 2 cases here. *)
                      | T(t_r_hd) -> ( match f with
                                      | [] -> None (* nothing left in the fragment but the rule has more to match, 
                                                     so it mustn't be the correct one *)
                                      | f_hd::f_tl -> if f_hd = t_r_hd then 
                                                        match_rule pf r_tl a d f_tl (*this matches so keep going *)
                                                      else
                                                        None) (* this rule doesnt match, so stop trying. *)
                      | N(n_r_hd) -> find_rule pf n_r_hd (pf n_r_hd) (* got a nonterm so we need to find_rules for it...*)
                                               (match_rule pf r_tl a)
                                                d f) (* pass match_rule as our acceptor so we can recursively build up 
                                                      * a more complex acceptor (and start where we've left off here!)
                                                      *)

(* Given a nonterm symbol find the alt-list and try all of the possibilities. 
 *  return None on Fail, else Some(d,s)
 * Use "and" here because find_rule and match_rule both call each other recursively.
 * *)
and find_rule pf nt_symb alt_list a d f =
    match alt_list with
    | [] -> None (* none of the rules in the alt list worked *)
    | alt_hd::alt_tl -> ( let try_this_rule = (match_rule pf alt_hd a (List.append d [nt_symb, alt_hd]) f) in
                          match try_this_rule with
                          | None -> find_rule pf nt_symb alt_tl a d f (* that rule didnt work so try the next one. *)
                          | _ -> try_this_rule) (* rule worked so return the result *)


let rec parse_prefix gram a f =
    match gram with
      (start, pf) -> find_rule pf start (pf start) a [] f
    




