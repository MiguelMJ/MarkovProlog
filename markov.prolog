:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(random)).
:- use_module(library(pcre)).
:- use_module(library(dicts)).
:- use_module(library(readutil)).

word_follows(F, S, [], [cons(F, [S])]).
word_follows(F, S, [cons(F, WL) | L], [cons(F, [S|WL]) | L]) :- !.
word_follows(F, S, [H|Rest], [H|X]) :- word_follows(F, S, Rest, X).

consecutive_list([], X, X).
consecutive_list([_], X, X).
consecutive_list([Prev, Curr |L], N, M) :-
    word_follows(Prev, Curr, N, N2),
    consecutive_list([Curr | L], N2, M)
    .

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
   
cons_freq(cons(F, WL), prcons(F, P)) :- freq(WL, P).

matrix_from_words(Words, Matrix) :-
    consecutive_list(Words, [], M),
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
    
gen_next_word([prcons(Prev, Probs)|_], Prev, Next) :-
    roulette(Probs, Next),
    !
    .
gen_next_word([_|T], Prev, Next) :- gen_next_word(T, Prev, Next).

process_match(Match, V0, V1) :-
    get_dict(0, Match, X),
    string_lower(X,Y),
    append(V0,[Y],V1)
    .

text_to_list(Text, List):-
    % re_foldl(process_match, "(\\w+)|([^\\w\\s]+)", Text, [], List, [])
    re_foldl(process_match, "(\\w+)|([,.;:?!]+)", Text, [], List, [])
    .


produce(Matrix, Result, First, Acc) :-
    gen_next_word(Matrix, First, Next),
    (
        Next \= ".",
        append(Acc,[Next],Acc2),
        produce(Matrix, Result, Next, Acc2)
    );
    (
        Next = ".",
        append(Acc,[Next], Result)
    )
    .
    
join_word(W, Acc, Res) :-
    string_concat(W, " ", W2),
    string_concat(Acc, W2, Res)
    .
    
main():-
    read_file_to_string('test.md', TextPre, []),
    string_concat(":::::::::: ", TextPre, Text),
    text_to_list(Text, List),
    matrix_from_words(List, Matrix),
    %trace,
    produce(Matrix, Result, "::::::::::", []),
    foldl(join_word, Result, "", W),
    print(W),
    !
    .
