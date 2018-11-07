:- use_module(trace_exps_common).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- EVENTS AND THEIR TYPE --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PRINT WRITER JAVA
event(open_file).
event(modify).
event(close_file).

of_type(open_file, ot).
of_type(modify, mt).
of_type(close_file, ct).

%%%%%%%%%%%%%%%%%%%
%% --- TESTS --- %%
%%%%%%%%%%%%%%%%%%%

%% ------------ PRINT WRITER JAVA ----------- %%

%% all possible behaviours
test(test0, Length, Traces) :- 
		C = annot(ct:eps, file_closed),
		M = C\/(mt:M),
		PW = annot(ot:annot(M,file_opened), start),
		bounded_accept_all_annot(PW, Length, Traces).

test(test01, Length, Traces) :- 
		C = ct:(eps\/C),
		M = (C\/(mt:M)),
		PW = ot:M,
		bounded_accept_all(PW, Length, Traces).

test(test02, Length, Traces) :- 
		PW = ot:ot:mt:mt:mt:mt:ct:mt:ct:eps,
		bounded_accept_all(PW, Length, Traces).

%% wrong behaviour (\/ PW in trace M and C in trace PW)
test(test1, Length, Traces) :- 
		C = annot((ct:(eps\/C)), file_closed),
		M = annot((C\/(mt:M)\/PW), file_opened),
		PW = annot(C\/(ot:M), file_opened),
		bounded_accept_all_annot(PW, Length, Traces).

