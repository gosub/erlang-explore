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
eval(TM, _) ->
    {ok, "Command not found.", TM}.


print_machine(TM) ->
    {ok, turing_machine:to_string(TM), TM}.


step_machine(TM) ->
    TM2 = turing_machine:step(TM),
    {ok, turing_machine:to_string(TM2), TM2}.
