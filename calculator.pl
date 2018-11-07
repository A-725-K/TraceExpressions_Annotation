:- use_module(trace_exps_common).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% --- EVENTS AND THEIR TYPE --- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% SIMPLE CALCULATOR
event(off).
event(digit).
event(canc).
event(operation).
event(equals).

of_type(E,E).

%%%%%%%%%%%%%%%%%%%
%% --- TESTS --- %%
%%%%%%%%%%%%%%%%%%%

%% ------------ SIMPLE CALCULATOR ----------- %%

%% all possible behaviours
test(test0, Length, Traces) :-
    Always = (off:Off)\/(canc:Operand1),
    Start = annot(Always\/(digit:Operand1), start),
    Off = annot(eps, off),
    Result = annot(Always\/(digit:Operand1), result),
    Operand2 = annot(Always\/(digit:Operand2)\/(equals:Result), operand2),
    Operand1 = annot(Always\/(digit:Operand1)\/(operation:Operation), operand1),
    Operation = annot(Always\/(digit:Operand2), operation),
    bounded_accept_all_annot(Start, Length, Traces).

 
test(test1, Length, Traces) :-
    Always = (off:Off)\/(canc:Operand1),
    Start = annot(Always\/Operand1, start),
    Off = annot(eps, off),
    Result = annot(Always\/(digit:Operand1), result),
    Operand2 = annot(Always\/(digit:Operand2)\/(equals:Result), operand2),
    Operand1 = annot(Always\/(digit:(Operand1\/(operation:Operation))), operand1),
    Operation = annot(Always\/(digit:Operand2), operation),
    bounded_accept_all_annot(Start, Length, Traces).

