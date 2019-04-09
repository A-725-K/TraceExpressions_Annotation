%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- EVENTS AND THEIR TYPE --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% SIMPLE TEST
event(a1).
event(a2).
event(c).

of_type(T,T).

test(t0, Len, Tr) :-
	T2 = annot(c:T1, cState),
	T1 = annot(a1:a2:T2, aState),
	bounded_accept_all_annot(T1, Len, Tr).
