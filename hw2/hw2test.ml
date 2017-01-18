type simplegrammar_nt = EXPR | A | B | C | D | E
let simple_grammar = (EXPR, function
                            | EXPR -> [[N A]]
                            | A -> [[N B;N C]]
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

let cyclical_grammar = (EXPR, function
                              | EXPR -> [[N D]]
                              | A -> [[N B]]
                              | B -> [[N C]]
                              | C -> [[N A]]
                              | D -> [[T "not used"]]
                              | E -> [[T "eterm"]])

let test_2  = parse_prefix cyclical_grammar accept_all ["eterm"] = None;;

