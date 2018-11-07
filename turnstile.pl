:- use_module(trace_exps_common).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- EVENTS AND THEIR TYPE --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TURNSTILE
event(ticket).
event(push).

of_type(ticket, tt).
of_type(push, pt).

%%%%%%%%%%%%%%%%%%%
%% --- TESTS --- %%
%%%%%%%%%%%%%%%%%%%

%% ------------ TURNSTILE ----------- %%

%% all possible behaviours
test(test0, Length, Traces) :-
		UL = annot(pt:LK, unlocked),
		LK = annot(END\/(tt:UL), locked),
		END = annot(eps, off),
		TRN = annot(LK, start),
		bounded_accept_all_annot(TRN, Length, Traces).
