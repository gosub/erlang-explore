-module(tmsh).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([run/1]).

run(TM) ->
    mini_repl:run("tmsh> ", fun tmsh_eval/2, TM).

tmsh_eval(_, Input) when Input=:="q" orelse Input=:="quit" ->
    quit;
tmsh_eval(TM, Input) when Input=:="p" ->
    {ok, turing_machine:to_string(TM), TM};
tmsh_eval(TM, Input) when Input=:="n" ->
    TM2 = turing_machine:step(TM),
    {ok, turing_machine:to_string(TM2), TM2};
tmsh_eval(TM, _) ->
    {ok, "Command not found.", TM}.

