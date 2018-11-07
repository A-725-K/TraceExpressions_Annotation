:- use_module(trace_exps_common).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- EVENTS AND THEIR TYPE --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ITERATORS JAVA
event(has_next(t)).
event(has_next(f)).
event(next).

of_type(has_next(t), hnt).
of_type(has_next(f), hnf).
of_type(next, nt).

%%%%%%%%%%%%%%%%%%%
%% --- TESTS --- %%
%%%%%%%%%%%%%%%%%%%

%% ------------ ITERATORS JAVA ----------- %%

%% all possible behaviours
test(test0, Length, Traces) :-
		O = annot(eps\/(nt:IT), step),
		T = eps\/(hnt:T),
		F = eps\/(hnf:F),
		IT = (annot((hnt:(T*O)), not_finished)\/annot(hnf:F, finished)),
		bounded_accept_all_annot(annot(IT, start), Length, Traces).

