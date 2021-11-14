-module(tmsh).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([run/1]).


run(TM) ->
    mini_repl:run("tmsh> ", fun eval/2, TM).


eval(_, Input) when Input=:="q" orelse Input=:="quit" ->
    quit;
eval(TM, Input) when Input=:="p" ->
    print_machine(TM);
eval(TM, Input) when Input=:="n" ->
    step_machine(TM);
eval(TM, Input) when Input=:="ns" ->
    step_machine_till_next_state(TM);
eval(TM, _) ->
    {ok, "Command not found.", TM}.


print_machine(TM) ->
    {ok, turing_machine:to_string(TM), TM}.


step_machine(TM) ->
    TM2 = turing_machine:step(TM),
    {ok, turing_machine:to_string(TM2), TM2}.


step_machine_till_next_state(TM) ->
    step_machine_till_next_state(TM, turing_machine:state(TM)).

step_machine_till_next_state(TM, FirstState) ->
    CurrentState = turing_machine:state(TM),
    case CurrentState =:= FirstState of
	true ->
	    TM2 = turing_machine:step(TM),
	    step_machine_till_next_state(TM2, FirstState);
	false ->
	    {ok, turing_machine:to_string(TM), TM}
    end.
