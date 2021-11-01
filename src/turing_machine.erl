-module(turing_machine).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([new/2, new/3, step/1, steps/2, to_string/1]).

-type state() :: string() | atom().
-type cell_match() :: turing_tape:cell() | any.
-type table_key() :: {state(), cell_match()}.
-type table_value() :: {[turing_tape:command()], state()}.
-type rule_table() :: #{table_key() := table_value()}.
-type rule_list() :: [state() | cell_match() | [turing_tape:command()]].
-type turing_machine() :: {rule_table(), state(), turing_tape:tape()}.

-spec new(rule_table(), state()) -> turing_machine().
new(Table, InitState) ->
    new(Table, InitState, turing_tape:new()).

-spec new(rule_table(), state(), turing_tape:tape()) -> turing_machine().
new(Table, InitState, Tape) ->
    {rule_table_from_list(Table), InitState, Tape}.

step({Table, State, Tape}) ->
    Read = turing_tape:read(Tape),
    {Actions, NewState} = match(State, Read, Table),
    NewTape = turing_tape:eval_list(Tape, Actions),
    {Table, NewState, NewTape}.

steps(Machine, 0) ->
    Machine;
steps(Machine, N) when is_integer(N) andalso N > 0 ->
    M2 = step(Machine),
    steps(M2, N-1).

match(State, Read, Table) ->
    case maps:find({State, Read}, Table) of
	{ok, Result} ->
	    Result;
	error ->
	    {ok, Result} = maps:find({State, any}, Table),
	    Result
    end.

to_string({_, State, Tape}) ->
    StateStr = lists:concat([State]),
    TapeStr = turing_tape:to_string(Tape),
    "state: " ++ StateStr ++ ", tape: " ++ TapeStr.

rule_table_from_list(List) ->
    rule_table_from_list(List, #{}).

rule_table_from_list([], AccMap) ->
    AccMap;
rule_table_from_list([State, Read, Instructions, NextState| Rest], AccMap) ->
    M2 = maps:put({State, Read}, {Instructions, NextState}, AccMap),
    rule_table_from_list(Rest, M2).
