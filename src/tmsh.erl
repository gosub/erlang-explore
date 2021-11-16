-module(tmsh).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([run/1]).


run(TM) ->
    mini_repl:run("tmsh> ", fun eval/2, {TM, 0}).


eval(_, Input) when Input=:="q" orelse Input=:="quit" ->
    quit;
eval(ReplState, Input) when Input=:="p" ->
    print_machine(ReplState);
eval(ReplState, Input) when Input=:="n" ->
    step_machine(ReplState);
eval(ReplState, Input) when Input=:="ns" ->
    step_machine_till_next_state(ReplState);
eval(ReplState, _) ->
    {ok, "Command not found.", ReplState}.


print_machine(ReplState={TM, _}) ->
    {ok, turing_machine:to_string(TM), ReplState}.


step_machine({TM, Counter}) ->
    TM2 = turing_machine:step(TM),
    {ok, turing_machine:to_string(TM2), {TM2, Counter+1}}.


step_machine_till_next_state(ReplState={TM, _}) ->
    step_machine_till_next_state(ReplState, turing_machine:state(TM)).

step_machine_till_next_state(TM, FirstState) ->
    CurrentState = turing_machine:state(TM),
    case CurrentState =:= FirstState of
	true ->
	    TM2 = turing_machine:step(TM),
	    step_machine_till_next_state(TM2, FirstState);
	false ->
	    {ok, turing_machine:to_string(TM), TM}
    end.
