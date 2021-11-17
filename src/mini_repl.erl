-module(mini_repl).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([run/2, run/3, echo_eval/1, inc_eval/2]).

-type stateless_eval_fun() :: fun((string()) -> {ok, string()} | quit | {error, string()}).


-spec run(string(), stateless_eval_fun()) -> none().
run(Prompt, EvalStateless) ->
    without_echo(fun run_repl_io/2, [Prompt, EvalStateless]).

run(Prompt, EvalStateful, InitState) ->
    without_echo(fun run_repl_io/3, [Prompt, EvalStateful, InitState]).


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


without_echo(F, Args) ->
    {echo, PreviousState} = lists:keyfind(echo, 1, io:getopts()),
    io:setopts([{echo, false}]),
    Result = apply(F, Args),
    io:setopts([{echo, PreviousState}]),
    Result.


echo_eval(S) when S=:="q" orelse S=:="quit" ->
    quit;
echo_eval(Str) ->
    {ok, Str}.

inc_eval(_, S) when S=:="q" orelse S=:="quit" ->
    quit;
inc_eval(_, "0") ->
    {ok, "0", 0};
inc_eval(N, _) ->
    {ok, integer_to_list(N+1), N+1}.
