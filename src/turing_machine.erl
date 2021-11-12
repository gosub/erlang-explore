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


%% match/3: find the correct state transition in the rule table
%%
%% Given the machine internal state and the symbol under the cursor,
%% returns the tuple containing the list of actions to perform on the
%% tape and the next machine state. In the rule table there could be
%% the atom 'any' in place of the symbol to find and this matches any
%% symbol found.

-spec match(state(), turing_tape:cell(), rule_table()) -> match_result().
match(State, Read, Table) ->
    case maps:find({State, Read}, Table) of
	{ok, Result} ->
	    Result;
	error ->
	    {ok, Result} = maps:find({State, any}, Table),
	    Result
    end.


%% to_string/1: return the string representation of a turing machine
%%
%% The final string has two parts concatenated: the internal state of
%% the machine and the string representation of the tape. All is
%% written on a single line.

-spec to_string(turing_machine()) -> string().
to_string({_, State, Tape}) ->
    StateStr = lists:concat([State]),
    TapeStr = turing_tape:to_string(Tape),
    "state: " ++ StateStr ++ ", tape: " ++ TapeStr.


%% rule_table_from_list/1: create a rule table map from a rule table list
%%
%% Helper function to adiuvate the init of a turing machine. Instead of
%% having to declare a rule table map like this:
%%   {state, symbol} => {[instructions], next_state}
%% it should be possible to use a list like this:
%%   [state1, symbol1, [instructions1], next_state1,
%%    state2, symbol2, [instructions2], next_state2, ... ]

-spec rule_table_from_list(rule_list()) -> rule_table().
rule_table_from_list(List) ->
    rule_table_from_list(List, #{}).


%% rule_table_from_list/2: helper functions for rule_table_from_list/1
%%
%% This version has a map where it accumulates every 4 entries in the
%% list in the form {el1, el2} => {el3, el4}.
%% If number of elements in the input list is not a multiple of 4, the
%% function fails with an error.

-spec rule_table_from_list(rule_list(), rule_table()) -> rule_table().
rule_table_from_list([], AccMap) ->
    AccMap;
rule_table_from_list([State, Read, Instructions, NextState| Rest], AccMap) ->
    M2 = maps:put({State, Read}, {Instructions, NextState}, AccMap),
    rule_table_from_list(Rest, M2).
