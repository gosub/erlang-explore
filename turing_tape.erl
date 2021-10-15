-module(turing_tape).

-export([new/0, left/1, right/1, write/2, clear/1, read/1,
	 eval/2, eval_list/2, to_list/1, to_string/1]).

-type cell_value() :: atom() | string().
-type cell() :: {cell_value()} | {}.
-type tape() :: {cell(), [cell()], [cell()]}.
-type command() :: left | right | clear | {write, cell_value()}.

-spec new() -> tape().
new() ->
    {{}, [], []}.

-spec left(tape()) -> tape().
left({{}, [], []}) ->
    {{}, [], []};
left({Cursor, [], Right}) ->
    {{}, [], [Cursor|Right]};
left({{}, [X|Rest], []}) ->
    {X, Rest, []};
left({Cursor, [X|Rest], Right}) ->
    {X, Rest, [Cursor|Right]}.

-spec right(tape()) -> tape().
right({{}, [], []}) ->
    {{}, [], []};
right({Cursor, Left, []}) ->
    {{}, [Cursor|Left], []};
right({{}, [], [X|Rest]}) ->
    {X, [], Rest};
right({Cursor, Left, [X|Rest]}) ->
    {X, [Cursor|Left], Rest}.

-spec write(tape(), cell_value()) -> tape().
write({_, L, R}, X) ->
    {{X}, L, R}.

-spec clear(tape()) -> tape().
clear({_, L, R}) ->
    {{}, L, R}.

-spec read(tape()) -> empty | {ok, cell_value()}.
read({{}, _, _}) ->
    empty;
read({{X}, _, _}) ->
    {ok, X}.

-spec eval(tape(), command()) -> tape().
eval(Tape, left) ->
    left(Tape);
eval(Tape, right) ->
    right(Tape);
eval(Tape, clear) ->
    clear(Tape);
eval(Tape, {write, X}) ->
    write(Tape, X).

-spec eval_list(tape(), [command()]) -> tape().
eval_list(Tape, []) ->
    Tape;
eval_list(Tape, [Cmd|Rest]) ->
    T2 = eval(Tape, Cmd),
    eval_list(T2, Rest).

-spec to_list(tape()) -> [cell()].
to_list({X, Left, Right}) ->
    lists:reverse(Left) ++ [X] ++ Right.

-spec to_string(tape()) -> string().
to_string({Cur, Left, Right}) ->
    StrL = cell_list_to_str(lists:reverse(Left)),
    StrCur = cell_to_str(Cur),
    StrR = cell_list_to_str(Right),
    StrL ++ "[" ++ StrCur ++ "]" ++ StrR.

cell_to_str({}) ->
    " ";
cell_to_str({X}) when is_list(X) ->
    X;
cell_to_str({X}) ->
    lists:concat([X]).

cell_list_to_str(L) ->
    cell_list_to_str(L, "|").

cell_list_to_str(L, Sep) ->
    lists:concat(
      lists:join(Sep, lists:map(fun cell_to_str/1, L))).
