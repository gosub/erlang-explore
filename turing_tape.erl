-module(turing_tape).

-export([new/0, left/1, right/1, write/2, clear/1, read/1,
	 eval/2, eval_list/2, to_list/1, to_string/1]).

-type cell_value() :: atom() | string().
-type cell() :: {cell_value()} | {}.
-type tape() :: {cell(), [cell()], [cell()]}.

new() ->
    {{}, [], []}.

left({{}, [], []}) ->
    {{}, [], []};
left({Cursor, [], Right}) ->
    {{}, [], [Cursor|Right]};
left({{}, [X|Rest], []}) ->
    {X, Rest, []};
left({Cursor, [X|Rest], Right}) ->
    {X, Rest, [Cursor|Right]}.

right({{}, [], []}) ->
    {{}, [], []};
right({Cursor, Left, []}) ->
    {{}, [Cursor|Left], []};
right({{}, [], [X|Rest]}) ->
    {X, [], Rest};
right({Cursor, Left, [X|Rest]}) ->
    {X, [Cursor|Left], Rest}.

write({_, L, R}, X) ->
    {{X}, L, R}.

clear({_, L, R}) ->
    {{}, L, R}.

read({{}, _, _}) ->
    empty;
read({{X}, _, _}) ->
    {ok, X}.

eval(Tape, left) ->
    left(Tape);
eval(Tape, right) ->
    right(Tape);
eval(Tape, clear) ->
    clear(Tape);
eval(Tape, {write, X}) ->
    write(Tape, X).

eval_list(Tape, []) ->
    Tape;
eval_list(Tape, [Cmd|Rest]) ->
    T2 = eval(Tape, Cmd),
    eval_list(T2, Rest).

to_list({X, Left, Right}) ->
    lists:reverse(Left) ++ [X] ++ Right.

to_string(Tape) ->
    L = lists:map(fun cell_to_str/1, to_list(Tape)),
    "|" ++ lists:concat(lists:join("|", L)) ++ "|".

cell_to_str({}) ->
    " ";
cell_to_str({X}) when is_list(X) ->
    X;
cell_to_str({X}) ->
    lists:concat([X]).
