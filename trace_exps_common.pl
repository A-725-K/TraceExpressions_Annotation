%to create the module
:- module(annot_trace_exp, [bounded_accept_all_annot/3, bounded_accept_all/3]).

%% to print long lists
:- set_prolog_flag(answer_write_options, [max_depth(0), spacing(next_argument)]).

:- use_module(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- TRACE EXPRESSIONS ANNOTATION --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_list([], _) :- !.
print_list([A|T], N) :-
		write(N), 
		(
			member(*, A) ->
				writeln(') trace:') ;
				writeln(') prefix:')
		),
		pretty_print(A, start),
		N1 is N+1,
		print_list(T, N1).

pretty_print([], Seq) :- nl, writeln(Seq), nl, !. %not in semantics
pretty_print([*], Seq) :-  nl, writeln(Seq), nl, !. %in semantics
pretty_print([(E, annot(TE, A))|T], Seq) :-
		write('event -> '), writeln(E),
		write('annotation -> '), writeln(A), 
		write('trace expression -> '), writeln(annot(TE, A)), nl,
		Seq1 = Seq-E,
		pretty_print(T, Seq1).

%% generic annotation with annot term
next(annot(TE, _), E, annot(TE1, A1)) :- 
		/*writeln('#########################'),
		write('TE_1 ----> '), writeln(TE),
		write('TE1_1 ----> '), writeln(TE),
		writeln('#########################'), nl, nl,*/
		next(TE, E, annot(TE1, A1)).
	
%% generic annotation TE != annot(...) 
next(annot(TE, A), E, annot(TE1, A)) :-
		next(TE, E, TE1),
		/*writeln('*************************'),
		write('TE_2 ----> '), writeln(TE),
		write('TE1_2 ----> '), writeln(TE),
		writeln('*************************'), nl, nl,
		*/TE1 \= annot(_, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- TRACE EXPRESSIONS --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% prefix
next(Theta:TrEx, E, TrEx) :-
		of_type(E, Theta).

%% or-l
next(T1\/_, E, T1_1) :- 
		next(T1, E, T1_1).

%% or-r
next(_\/T2, E, T2_1) :- 
		next(T2, E, T2_1).

%% shuffle-l
next(T1|T2, E, T) :- 
		next(T1, E, T1_1), 
		shuffle_opt(T1_1, T2, T).

%% shuffle-r
next(T1|T2, E, T) :- 
		next(T2, E, T2_1), 
		shuffle_opt(T1, T2_1, T).

%% cat-l
next(T1*T2, E, T) :-
		next(T1, E, T1_1),
		concat_opt(T1_1, T2, T).

%% cat-r
next(T1*T2, E, T) :-
		may_halt(T1),
		next(T2, E, T).

%% and
next(T1/\T2, E, T) :- 
		next(T1, E, T1_1), 
		next(T2, E, T2_1), 
		and_opt(T1_1, T2_1, T).

%% filter
next(ET >> T1, E, ET >> T2)  :- 
		event(E),
		(has_type(E, ET) *-> next(T1, E, T2) ; T2 = T1). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- HALTING RULES --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% eps-annot
may_halt(annot(T, _)) :-
		may_halt(T).

%% eps-empty
may_halt(eps) :- !.

%% eps-or
may_halt(T1\/T2) :-
		(may_halt(T1), !;
		may_halt(T2)).

%% eps-shuffle
may_halt(T1|T2) :- 
		may_halt(T1), 
		may_halt(T2).

%% eps-cat
may_halt(T1*T2) :- 
		may_halt(T1), 
		may_halt(T2).

%% eps-and
may_halt(T1/\T2) :- 
		may_halt(T1), 
		may_halt(T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- DOESN'T HALTING RULES --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% annot
does_not_halt(annot(T, _)) :-
		does_not_halt(T).

%% prefix
does_not_halt(_:_).

%% or
does_not_halt(T1\/T2) :- 
		does_not_halt(T1),
		does_not_halt(T2).

%% shuffle
does_not_halt(T1|T2) :- 
		(does_not_halt(T1), !;
		does_not_halt(T2)).

%% cat
does_not_halt(T1|T2) :- 
		(does_not_halt(T1), !;
		does_not_halt(T2)).
%% and
does_not_halt(T1|T2) :- 
		(does_not_halt(T1), !;
		does_not_halt(T2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- OPTIMIZATIONS --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

shuffle_opt(eps, T, T) :- !.
shuffle_opt(T, eps, T) :- !.
shuffle_opt((T1_l|T1_r), T2, (T1_l|(T1_r|T2))) :- !.
shuffle_opt(T1, T2, T1|T2).

concat_opt(eps, T, T) :- !.
concat_opt(T, eps, T) :- !.
concat_opt((T1_l*T1_r), T2, (T1_l*(T1_r*T2))) :- !.
concat_opt(T1, T2, T1*T2).

and_opt(eps/\eps, eps) :- !.
and_opt((T1_l/\T1_r), T2, T1_l/\(T1_r/\T2)) :- !.
and_opt(T1, T2, T1/\T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- TRACE EXPRESSION SEMANTICS --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% trace expressions ending with * belongs to the semantics
accept(TE, [*], TE) :-
		may_halt(TE).
%% trace expressions ending with no * are just valid prefixes,
%% works only for trace expressions satisfying anticipation
accept(TE, [], TE) :-
		does_not_halt(TE),
		next(TE, _, _).
accept(TE_1, [FirstE|T], TE) :-
		next(TE_1, FirstE, TE_2),
		accept(TE_2, T, TE).
%% accept without residual expression
accept(TE, Tr) :-
		accept(TE, Tr, _TE).

%%%%%%%%%%%%%%%%%%%%
%% --- LIMITS --- %%
%%%%%%%%%%%%%%%%%%%%

%% with annotations
bounded_accept_annot(TE, Len, [*]) :-
		may_halt(TE),
		Len >= 0.
bounded_accept_annot(TE, 0, []) :- %% works only for trace expressions satisfying anticipation
		does_not_halt(TE),
		next(TE, _, _).
bounded_accept_annot(TE, Len, [(FirstE, NewTE)|T]) :-
		Len > 0,
		next(TE, FirstE, NewTE),
		Len_1 is Len - 1,
		bounded_accept_annot(NewTE, Len_1, T).

bounded_accept_all_annot(TE, Len, Traces) :-
		setof(Tr, bounded_accept_annot(TE, Len, Tr), Traces), !, print_list(Traces, 1).
bounded_accept_all_annot(_,_,[]).


%% without annotations
bounded_accept(TE, Len, [*]) :-
		may_halt(TE),
		Len >= 0.
bounded_accept(TE, 0, []) :- %% works only for trace expressions satisfying anticipation
		does_not_halt(TE),
		next(TE, _, _).
bounded_accept(TE, Len, [FirstE|T]) :-
		Len > 0,
		next(TE, FirstE, NewTE),
		writeln('#########################'),
		write('Event ----> '), writeln(FirstE),
		write('Trace Expression ----> '), writeln(TE),
		writeln('#########################'), nl, nl,
		Len_1 is Len - 1,
		bounded_accept(NewTE, Len_1, T).

bounded_accept_all(TE, Len, Traces) :-
		setof(Tr, bounded_accept(TE, Len, Tr), Traces), !.
bounded_accept_all(_,_,[]).
