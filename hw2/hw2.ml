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







