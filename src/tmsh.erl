-module(tmsh).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([run/1, test/0]).

-type repl_state() :: {turing_machine:turing_machine(), integer()}.
-type eval_output() :: {ok, string(), repl_state()} | quit.

-spec run(turing_machine:turing_machine()) -> none().
run(TM) ->
    mini_repl:run("tmsh> ", fun eval/2, {TM, 0}).


-spec eval(repl_state(), string()) -> eval_output().
eval(_, Input) when Input=:="q" orelse Input=:="quit" ->
    quit;
eval(ReplState, "p") ->
    print_machine(ReplState);
eval(ReplState, "n") ->
    step_machine(ReplState);
eval(ReplState, "ns") ->
    step_machine_till_next_state(ReplState);
eval(ReplState={_, Counter}, Input) when Input=:="c" ->
    {ok, io_lib:format("Step counter: ~p",[Counter]), ReplState};
eval(ReplState, _) ->
    {ok, "Command not found.", ReplState}.


-spec print_machine(repl_state()) -> eval_output().
print_machine(ReplState={TM, _}) ->
    {ok, turing_machine:to_string(TM), ReplState}.


-spec step_machine(repl_state()) -> eval_output().
step_machine({TM, Counter}) ->
    TM2 = turing_machine:step(TM),
    {ok, turing_machine:to_string(TM2), {TM2, Counter+1}}.


-spec step_machine_till_next_state(repl_state()) -> eval_output().
step_machine_till_next_state(ReplState={TM, _}) ->
    step_machine_till_next_state(ReplState, turing_machine:state(TM)).


-spec step_machine_till_next_state(repl_state(), turing_machine:state()) -> eval_output().
step_machine_till_next_state(ReplState, FirstState) ->
    {TM, Counter} = ReplState,
    CurrentState = turing_machine:state(TM),
    case CurrentState =:= FirstState of
	true ->
	    TM2 = turing_machine:step(TM),
	    step_machine_till_next_state({TM2, Counter+1}, FirstState);
	false ->
	    {ok, turing_machine:to_string(TM), ReplState}
    end.


-spec test() -> none().
test() ->
    run(turing_machines:make(turing_machines:sqrt_of_two())).
