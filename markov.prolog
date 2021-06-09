:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(random)).
:- use_module(library(pcre)).
:- use_module(library(dicts)).
:- use_module(library(readutil)).


join(_,[], []) :- !.
join(_, [R], R) :- !.
join(Sep, [H1,H2|T], Res) :-
    string_concat(H1,Sep,Hs),
    string_concat(Hs,H2,HsH),
    join(Sep, [HsH|T], Res)
    .
    
make_key(Ws,Key) :- 
    join("..", Ws, Str),
    atom_string(Key,Str).

word_follows(Ws, PairList, X) :- 
    append(W1, [W2], Ws),
    make_key(W1, K),
    append(PairList, [K-W2], X)
    % X = Dict.put(K, W2)
    .

divide_list(0, List, [], List).
divide_list(N,[H|T1],[H|T2], Rest) :- succ(N1,N), divide_list(N1, T1, T2, Rest).

consecutive_list(Level, Words, X, Y) :- 
    length(Words, N), 
    N =< Level,
    keysort(X, X2),
    group_pairs_by_key(X2,Y),
    !
    .
consecutive_list(Level, [H|T], N, M) :-
    succ(Level, S),
    divide_list(S, [H|T], Head,_),
    word_follows(Head, N, N2),
    consecutive_list(Level, T, N2, M)
    .

consecutive_list(Level, Words, Result) :- consecutive_list(Level, Words, [], Result).
    
grab([], _, [], []).
grab([H|L], H, [H|X], Y) :- grab(L, H, X, Y), !.
grab([H|L], E, X, [H|Y]) :- grab(L, E, X, Y).

grab([H|L], B, C) :- grab([H|L], H, B, C).

freq([],[], _).
freq([A|L],[cnt(A,RF)|Y], F) :-
   grab([A|L],As,K),
   length(As, N),
   RF is N/F,
   freq(K,Y, F)
   .
freq(A, B) :- 
    length(A, F),
    freq(A, B, F).
   
cons_freq(F-WL, F-P) :- freq(WL, P).

markov(Level, Words, Matrix) :-
    consecutive_list(Level, Words, M),
    maplist(cons_freq, M, Matrix)
    .

roulette([cnt(W, P)|_], W, X) :- X-P =< 0, !.
roulette([cnt(_,P)|L], W, X) :- 
    Y is X-P, 
    roulette(L, W, Y).
roulette(Probs, Word) :-
    random(X),
    roulette(Probs, Word, X)
    .
    
gen_next_word(Markov, Head, Next) :-
    make_key(Head, Key),
    member(Key-Probs, Markov),
    roulette(Probs, Next),
    !
    .

process_match(Match, V0, V1) :-
    get_dict(0, Match, X),
    string_lower(X,Y),
    append(V0,[Y],V1)
    .

text_to_list(Text, List):-
    re_compile("(\\w+)|([,.;:?!¡¿]+)", Re, [ucp(true)]),
    re_foldl(process_match, Re, Text, [], List, [])
    .


produce(Matrix, Result, [H|T], Acc) :-
    %trace,
    gen_next_word(Matrix, [H|T], Next),
    (
        Next \= ".",
        append(Acc,[Next],Acc2),
        append(T, [Next], NextHead),
        produce(Matrix, Result, NextHead, Acc2)
    );
    (
        Next = ".",
        append(Acc,[Next], Result)
    )
    .
    
make_guards(N, List, String):-
    length(List, N),
    maplist([_,X]>>(X=":::::"), _, List),
    foldl([E,X,Y]>>(string_concat(E," ",Es), string_concat(Es,X,Y)), List, "", String)
    .

main():-
    Level = 2,
    read_file_to_string('test.md', TextPre, []),
    make_guards(Level, GList, GStr),
    string_concat(GStr, TextPre, Text),
    text_to_list(Text, List),
    markov(Level, List, Matrix),
    %trace,
    produce(Matrix, Result, GList, []),
    join(" ", Result, W),
    print(W),
    !
    .
