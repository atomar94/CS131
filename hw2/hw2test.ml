type simplegrammar_nt = EXPR | A | B | C | D | E
let simple_grammar = (EXPR, function
                            | EXPR -> [[N A]]
                            | A -> [[N B;N C];[N E;N E]]
                            | B -> [[N C;N D;N E];[N A]]
                            | C -> [[T "cterm"]]
                            | D -> [[N C]]
                            | E -> [[T "eterm"]]
                     )
let accept_all derivation string = Some (derivation, string)

let test_1 = parse_prefix simple_grammar accept_all ["cterm";"cterm";"eterm";"cterm"] =  
    Some
    ([(EXPR, [N A]); 
    (A, [N B; N C]);
    (B, [N C; N D; N E]); 
    (C, [T "cterm"]);
    (D, [N C]); 
    (C, [T "cterm"]); 
    (E, [T "eterm"]); 
    (C, [T "cterm"])]
    ,[]);;

let less_simple_grammar = (EXPR, function
                              | EXPR -> [[N A]]
                              | A -> [[N B];[N E]]
                              | B -> [[N C];[N D];[N A]]
                              | C -> [[N D; N D];[N B;N A;N C]]
                              | D -> [[T "dterm"];[N A]]
                              | E -> [[]]);;

(*let test_2  = parse_prefix cyclical_grammar accept_all ["eterm"];;*)
let test_2 = parse_prefix less_simple_grammar accept_all ["dterm";"dterm"] =
Some
   ([(EXPR, [N A]); (A, [N B]); (B, [N C]); (C, [N D; N D]);
        (D, [T "dterm"]); (D, [T "dterm"])],
            []);;
