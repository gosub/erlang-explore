-module(mini_repl).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([run/2, run/3, echo_eval/1, inc_eval/2]).

-type stateless_eval_fun() :: fun((string()) -> {ok, string()} | quit | {error, string()}).
-type stateful_eval_fun() :: fun((any(), string()) -> {ok, string(), any()} | quit | {error, string()}).

-export_type([stateful_eval_fun/0, stateless_eval_fun/0]).


-spec run(string(), stateless_eval_fun()) -> none().
run(Prompt, EvalStateless) ->
    without_echo(fun run_repl_io/2, [Prompt, EvalStateless]).


-spec run(string(), stateful_eval_fun(), any()) -> none().
run(Prompt, EvalStateful, InitState) ->
    without_echo(fun run_repl_io/3, [Prompt, EvalStateful, InitState]).


-spec run_repl_io(string(), stateless_eval_fun()) -> none().
run_repl_io(Prompt, EvalStateless) ->
    Line = io:get_line(Prompt),
    case EvalStateless(string:chomp(Line)) of
	{ok, Output} ->
	    io:format("~s~n", [Output]),
	    run_repl_io(Prompt, EvalStateless);
	quit ->
	    quit;
	{error, Error} ->
	    io:format("Error: ~p\n", [Error])
    end.


-spec run_repl_io(string(), stateful_eval_fun(), any()) -> none().
run_repl_io(Prompt, EvalStateful, State) ->
    Line = io:get_line(Prompt),
    case EvalStateful(State, string:chomp(Line)) of
	{ok, Output, NewState} ->
	    io:format("~s~n", [Output]),
	    run_repl_io(Prompt, EvalStateful, NewState);
	quit ->
	    quit;
	{error, Error} ->
	    io:format("Error: ~s~n", [Error])
    end.


-spec without_echo(fun(), [any()]) -> any().
without_echo(F, Args) ->
    {echo, PreviousState} = lists:keyfind(echo, 1, io:getopts()),
    io:setopts([{echo, false}]),
    Result = apply(F, Args),
    io:setopts([{echo, PreviousState}]),
    Result.


-spec echo_eval(string()) -> {ok, string()} | quit.
echo_eval(S) when S=:="q" orelse S=:="quit" ->
    quit;
echo_eval(Str) ->
    {ok, Str}.


-spec inc_eval(integer(), string()) -> quit | {ok, string(), integer()}.
inc_eval(_, S) when S=:="q" orelse S=:="quit" ->
    quit;
inc_eval(_, "0") ->
    {ok, "0", 0};
inc_eval(N, _) ->
    {ok, integer_to_list(N+1), N+1}.
