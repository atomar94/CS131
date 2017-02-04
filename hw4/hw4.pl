/*get the first elem in a list*/
first([F | _], F).
first([], []).


signal_morse([], []). /* base case */


/*8 0s -> 7 0s    (this accounts for all >7 0s*/
signal_morse([0, 0, 0, 0, 0, 0, 0, 0 | T], M):-
    signal_morse([0, 0, 0, 0, 0, 0, 0 | T], M).


/*7 zeroes (terminal -> #) */
signal_morse([0, 0, 0, 0, 0, 0, 0 | T], M):-
    (first(T, 1) ; first(T, [])), /*make sure this is all the 0s there are.*/
    signal_morse(T, M_future),
    append([#], M_future, M).


/*4 zeroes -> 3 zeroes*/
signal_morse([0, 0, 0, 0 | T], M):-
    (first(T, 1) ; first(T, [])),
    signal_morse([0, 0, 0 | T], M).


/*6 zeroes -> 7 zeroes */
signal_morse([0, 0, 0, 0, 0, 0 | T], M):-
    (first(T, 1) ; first(T, []) ),
    signal_morse([0, 0, 0, 0, 0, 0, 0 | T], M).


/* 5 zeroes -> 3 zeroes*/
signal_morse([0, 0, 0, 0, 0 | T], M):-
    (first(T, 1) ; first(T, [])),
    signal_morse([0, 0, 0 | T], M).

/*5 0s -> 7 0s*/
signal_morse([0, 0, 0, 0, 0 | T], M):-
    ( first(T, 1) ; first(T, []) ),
    signal_morse([0, 0, 0, 0, 0, 0, 0 | T], M).

/*3 0s (terminal-> ^) */
signal_morse([0, 0, 0 | T], M):-
    (first(T, 1) ; first(T, [])),
    signal_morse(T, M_future),
    append([^], M_future, M).

/* 2 0s -> 3 0s*/
signal_morse([0, 0 | T], M):-
    (first(T, 1) ; first(T, [])),
    signal_morse([0, 0, 0 | T], M).
    

/* 2 0s -> 1 0s*/
signal_morse([0, 0 | T], M):-
    (first(T, 1) ; first(T, [])),
    signal_morse([0 | T], M).


/*1 0s (separates . and -, but we can just strip it and continue */
signal_morse([0 | T], M):-
    ( first(T, 1) ; first(T, []) ),
    signal_morse(T, M).


/*3 1s (terminal) */
signal_morse([1, 1, 1 | T], M):-
    ( first(T, 0) ; first(T, []) ),
    signal_morse(T, M_future),
    append(['-'], M_future, M).


/*4 1s -> 3 1s. here we don't check the first elem in T so we can
* use this one rule to catch any number of 1s */
signal_morse([1, 1, 1, 1 | T], M):-
    signal_morse([1, 1, 1 | T], M).


/*2 1s -> 1 1s*/
signal_morse([1, 1 | T], M):-
    ( first(T, 0) ; first(T, []) ),
    signal_morse([1 | T], M).


/*2 1s -> 3 1s*/
signal_morse([1, 1 | T], M):-
    ( first(T, 0) ; first(T, []) ),
    signal_morse([1, 1, 1 | T], M).


/*1 1s (terminal) */
signal_morse([1 | T], M):-
    ( first(T, 0) ; first(T, []) ),
    signal_morse(T, M_future),
    append(['.'], M_future, M).

/**********************************
*   signal_message_inner implementation
***********************************/


/* we need this to act as a wrapper because of we way we account for errors.
 *
 * we pass the output of signal_message_inner into the error_handler() function
 * but we only want to call that function once, and so we couldn't put in .._inner
 * so instead we have signal_message be a wrapper function so we can insure to call
 * error_handler only once per solution.
 *
 * */
signal_message(L, M):-
    signal_message_inner(L, M_inner),
    error_handler(M_inner, [], M).

/* base case */
signal_message_inner([], []). 

/*if we get passed an unformatted string of numbers 
* then we should pass that through signal_morse first */
signal_message_inner([1 | T], M):-
    signal_morse([1 | T], M_morse),
    signal_message_inner(M_morse, M).

signal_message_inner([0 | T], M):-
    signal_morse([0 | T], M_morse),
    signal_message_inner(M_morse, M).

/*get rid of ^*/
signal_message_inner([^ | T], M):-
    signal_message_inner(T, M).

/*move beyond # but keep it*/
signal_message_inner([# | T], M):-
   signal_message_inner(T, M_future),
   append([#], M_future, M).


/*catch morse of a single dih dah */
signal_message_inner([H1 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).

/*catch morse of 2 dih dah*/
signal_message_inner([H1, H2 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1, H2]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).

/*catch morse of 3 dih dah*/
signal_message_inner([H1, H2, H3 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1, H2, H3]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).

/*catch morse of 4 dih dah*/
signal_message_inner([H1, H2, H3, H4 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1, H2, H3, H4]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).

/*catch morse of 5 dih dah*/
signal_message_inner([H1, H2, H3, H4, H5 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1, H2, H3, H4, H5]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).

/*catch morse of 6 dih dah*/
signal_message_inner([H1, H2, H3, H4, H5, H6 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1, H2, H3, H4, H5, H6]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).

/*catch morse of 7 dih dah*/
signal_message_inner([H1, H2, H3, H4, H5, H6, H7 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1, H2, H3, H4, H5, H6, H7]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).

/*catch morse of 8 dih dah (error msg)*/
signal_message_inner([H1, H2, H3, H4, H5, H6, H7, H8 | T], M):-
    (first(T, ^) ; first(T, #) ; first(T, [])),
    morse(Symbol, [H1, H2, H3, H4, H5, H6, H7, H8]),
    signal_message_inner(T, M_future),
    append([Symbol], M_future, M).


/*************************************
*   Error Handler Implementation
**************************************/

/*
*   lets go through the list and keep track of our current word
*   if we see a # we add the current word to out output list and
*   then start a new current_word_list
*   if we see an error 
*/

/*base case*/
error_handler([], W, Output):-
    append(W, [], Output).

/*if we get a # in front then start searching for errors again and append
* our word we found to the output.
* */
error_handler([# | T], Current_word, Output):-
    error_handler(T, [#], Output_future),
    append(Current_word, Output_future, Output). %todo is this the right append ordering?

/*if we got an error and nothing is in the buffer then theres nothing to clear,
* so this error symbol can get appended to our output */
error_handler([error | T], [], Output):-
    error_handler(T, [], Output_future),
    append([error], Output_future, Output).

/*if we got an error and we have a word in the buffer (more than just a #)
* then clear buffer and keep going */
error_handler([error | T], [Current_word_H | Current_word_T], Output):-
    \+(first(Current_word_T, [])), /*the buffer has more stuff after the #*/
    error_handler(T, [], Output).

/*we got an error and we have only a hashtag in the buffer, flush the buffer to output*/
error_handler([error | T], [#], Output):-
    error_handler(T, [], Output_future),
    append([#, error], Output_future, Output).


/*if we found a non-special case (not a #, not an error) then add the symbol
* to the current word buffer and keep looking */
error_handler([H | T], Current_word, Output):-
    \+first([H], error), /*dont use this rule is H is "empty" (in case of backtrack), we have rules for that. */
    \+first([H], #), /*dont use this rule if H is #, we have a rule for that.*/
    append(Current_word, [H], New_word),
    error_handler(T, New_word, Output).




/* Morse codes I copy pasted from the spec */
morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).       % B
morse(c, [-,.,-,.]).       % C
morse(d, [-,.,.]).     % D
morse(e, [.]).         % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).       % F
morse(g, [-,-,.]).     % G
morse(h, [.,.,.,.]).       % H
morse(i, [.,.]).       % I
morse(j, [.,-,-,-]).       % J
morse(k, [-,.,-]).     % K or invitation to transmit
morse(l, [.,-,.,.]).       % L
morse(m, [-,-]).       % M
morse(n, [-,.]).       % N
morse(o, [-,-,-]).     % O
morse(p, [.,-,-,.]).       % P
morse(q, [-,-,.,-]).       % Q
morse(r, [.,-,.]).     % R
morse(s, [.,.,.]).     % S
morse(t, [-]).         % T
morse(u, [.,.,-]).     % U
morse(v, [.,.,.,-]).       % V
morse(w, [.,-,-]).     % W
morse(x, [-,.,.,-]).       % X or multiplication sign
morse(y, [-,.,-,-]).       % Y
morse(z, [-,-,.,.]).       % Z
morse(0, [-,-,-,-,-]).     % 0
morse(1, [.,-,-,-,-]).     % 1
morse(2, [.,.,-,-,-]).     % 2
morse(3, [.,.,.,-,-]).     % 3
morse(4, [.,.,.,.,-]).     % 4
morse(5, [.,.,.,.,.]).     % 5
morse(6, [-,.,.,.,.]).     % 6
morse(7, [-,-,.,.,.]).     % 7
morse(8, [-,-,-,.,.]).     % 8
morse(9, [-,-,-,-,.]).     % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)
