:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(random)).
:- use_module(library(pcre)).

corpus("corpus").
level(1).
guard(":::::").

join_list(Sep, L, J) :-
    length(L,X), 
    X < 2,
    J = L,
    !;
    L = [A,B|T],
    J = [A,Sep|X],
    join_list(Sep, [B|T], X)
    .
    
join(Sep, Words, Result):-
    maplist(string_codes, [Sep|Words], [SepCode|Codes]),
    join_list(SepCode, Codes, Codes2),
    flatten(Codes2,Pre),
    string_codes(Result,Pre)
    .
    
make_key(Ws,Key) :- 
    join("..", Ws, Str),
    atom_string(Key,Str).
    
word_follows(Ws, PairList, X) :-
    last(Ws,W2),
    findall(W1, append(W1, [W2], Ws), [W1]),
    make_key(W1, K),
    append(PairList, [K-W2], X)
    .

consecutive_list(Level, Words, Result) :- 
    consecutive_list(Level, Words, [], Result)
    .
consecutive_list(Level, Words, X, Y) :- 
    length(Words, N), 
    N =< Level,
    keysort(X, X2),
    group_pairs_by_key(X2,Y)
    .
consecutive_list(Level, L, N, M) :-
    prolog_current_frame(F), format('~d~n',[F]),
    L = [_|T],
    succ(Level, S),
    length(Head, S),
    append(Head, _, L),
    word_follows(Head, N, N2),
    consecutive_list(Level, T, N2, M)
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
    \+ guard(Next),
    !
    .

process_match(Match, V0, V1) :-
    get_dict(0, Match, X),
    string_lower(X,Y),
    append(V0,[Y],V1)
    .

text_to_list(Text, List):-
    re_compile("([^\\W_]+)|([,.;:?!¡¿]+)", Re, [ucp(true)]),
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
    maplist(guard, List),
    join(" ", List, String)
    .

text_from_corpus(Text) :-
    %trace,
    level(Level),
    corpus(Corpus),
    string_concat(Corpus, "/*",Path),
    expand_file_name(Path,Files),
    %maplist([X,Y]>>(corpus(C),join("/",[C,X], Y)),FilesAt,Files),
    make_guards(Level, _, Guards),
    %trace,
    %maplist([File,Body]>>(level(L),make_guards(L,_,Guards),read_file_to_string(File,Pre,[]), string_concat(Guards,Pre,Body)), Files, Texts),
    maplist([File,Body]>>(read_file_to_string(File,Body,[])), Files, Texts),
    join(Guards,[" "," "], SGuardsS),
    join(SGuardsS, [""|Texts], Text)
    .
    
main() :-
    level(Level),
    %read_file_to_string('test.md', TextPre, []),
    make_guards(Level, GList, _),
    %string_concat(GStr, TextPre, Text),
    text_from_corpus(Text),
    text_to_list(Text, List),
    length(List, WC),
    format('Building Markov chain from ~d words~n', [WC]),
    markov(Level, List, Markov),
    length(Markov, Nstates),
    format('Word count: ~d, Level ~d: ~d states~n', [WC, Level, Nstates]),
    produce(Markov, Result, GList, []),
    join(" ", Result, W),
    print(W),
    !
    .
