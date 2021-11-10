-module(turing_machine).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([new/2, new/3, step/1, steps/2, to_string/1]).

-type state() :: string() | atom().
-type cell_match() :: turing_tape:cell() | any.
-type match_result() :: {ok, table_value()} | error.
-type table_key() :: {state(), cell_match()}.
-type table_value() :: {[turing_tape:command()], state()}.
-type rule_table() :: #{table_key() := table_value()}.
-type rule_list() :: [state() | cell_match() | [turing_tape:command()]].
-type turing_machine() :: {rule_table(), state(), turing_tape:tape()}.


%% new/2: create a new turing machine from the rule table and initial state
%%
%% The rule table is a map where the keys are a tuple made of:
%%  - internal state of the machine
%%  - value that matches the cell under the cursor
%%
%% The values of the table are also a tuple, made of:
%%  - a list of commands to run when the rule matches
%%  - the new internal state of the turing machine
%%
%% The machine is initialized with an empty tape.

-spec new(rule_table(), state()) -> turing_machine().
new(Table, InitState) ->
    new(Table, InitState, turing_tape:new()).


%% new/3: create a new turing machine with an inital tape
%%
%% The machine is created with a specific initial tape.

-spec new(rule_table(), state(), turing_tape:tape()) -> turing_machine().
new(Table, InitState, Tape) ->
    {rule_table_from_list(Table), InitState, Tape}.

%% step/1: advance a turing machine by a single state transition
%%
%% Returns a new turing machine, where the internal state and the tape
%% are updated according to the transition table, the current state and
%% the symbol under the cursor.

-spec step(turing_machine()) -> turing_machine().
step({Table, State, Tape}) ->
    Read = turing_tape:read(Tape),
    {Actions, NewState} = match(State, Read, Table),
    NewTape = turing_tape:eval_list(Tape, Actions),
    {Table, NewState, NewTape}.

%% step/2: advance a turing machine by N state transitions
%%
%% Applies step/1 N times (specified by the second argument),
%% and returns the final turing machine.

-spec steps(turing_machine(), integer()) -> turing_machine().
steps(Machine, 0) ->
    Machine;
steps(Machine, N) when is_integer(N) andalso N > 0 ->
    M2 = step(Machine),
    steps(M2, N-1).

-spec match(state(), turing_tape:cell(), rule_table()) -> match_result().
match(State, Read, Table) ->
    case maps:find({State, Read}, Table) of
	{ok, Result} ->
	    Result;
	error ->
	    {ok, Result} = maps:find({State, any}, Table),
	    Result
    end.

-spec to_string(turing_machine()) -> string().
to_string({_, State, Tape}) ->
    StateStr = lists:concat([State]),
    TapeStr = turing_tape:to_string(Tape),
    "state: " ++ StateStr ++ ", tape: " ++ TapeStr.

-spec rule_table_from_list(rule_list()) -> rule_table().
rule_table_from_list(List) ->
    rule_table_from_list(List, #{}).

-spec rule_table_from_list(rule_list(), rule_table()) -> rule_table().
rule_table_from_list([], AccMap) ->
    AccMap;
rule_table_from_list([State, Read, Instructions, NextState| Rest], AccMap) ->
    M2 = maps:put({State, Read}, {Instructions, NextState}, AccMap),
    rule_table_from_list(Rest, M2).
