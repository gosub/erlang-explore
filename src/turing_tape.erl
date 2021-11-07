-module(turing_tape).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([new/0, left/1, right/1, write/2, clear/1, read/1,
	 eval/2, eval_list/2, to_list/1, to_string/1]).

-type non_empty_cell() :: string() | 0 | 1.
-type cell() :: {} | non_empty_cell().
-type tape() :: {cell(), [cell()], [cell()]}.
-type command() :: left | right | clear | {write, non_empty_cell()}.

-export_type([tape/0, cell/0, command/0]).


%% new/0: return a new empty tape
%%
%% Every tape is a tuple of:
%% - the content of the cell under cursor ({} is the empty cell)
%% - a list of cells to the left of the cursor
%% - a list of cells to the right of the cursor
%%
%% The convention is that an empty list represents a infinite list
%% of empty cells. The left list of cells is reversed, so that moving
%% through it is an operation that can be made on the head of the list.
%% This implementation is equivalent to a zipper data structure.

-spec new() -> tape().
new() ->
    {{}, [], []}.


%% left/1: return the input tape shifted one cell to the left
%%
%% Pops into the cursor cell the head of the list in the direction
%% of movement, and pushes the old cell content onto the head of the
%% list in the opposite direction.
%%
%% Pushing an empty cell to an empty list doesn't grow the list.

-spec left(tape()) -> tape().
left({{}, [], []}) ->
    {{}, [], []};
left({Cursor, [], Right}) ->
    {{}, [], [Cursor|Right]};
left({{}, [X|Rest], []}) ->
    {X, Rest, []};
left({Cursor, [X|Rest], Right}) ->
    {X, Rest, [Cursor|Right]}.


%% right/1: return the input tape shifted one cell to the right
%%
%% Same as left/1

-spec right(tape()) -> tape().
right({{}, [], []}) ->
    {{}, [], []};
right({Cursor, Left, []}) ->
    {{}, [Cursor|Left], []};
right({{}, [], [X|Rest]}) ->
    {X, [], Rest};
right({Cursor, Left, [X|Rest]}) ->
    {X, [Cursor|Left], Rest}.


%% write/2: return the input tape with the second argument written
%%          in cell under the cursor of the input tape

-spec write(tape(), non_empty_cell()) -> tape().
write({_, L, R}, X) ->
    {X, L, R}.


%% clear/1: return the input tape with the cell under the cursor cleared

-spec clear(tape()) -> tape().
clear({_, L, R}) ->
    {{}, L, R}.


%% read/1: return the content of the cell under the cursor of
%%         the input tape

-spec read(tape()) -> cell().
read({Cell, _, _}) ->
    Cell.


%% eval/1: apply to the input tape a command (second argument) and return
%%         the new modified tape
%%
%% Possible commands:
%% - left
%% - right
%% - clear
%% - {write, X}
%%
%% The first three commands are simple atoms, the last one is a tuple
%% with the 'write' atom as the first element

-spec eval(tape(), command()) -> tape().
eval(Tape, left) ->
    left(Tape);
eval(Tape, right) ->
    right(Tape);
eval(Tape, clear) ->
    clear(Tape);
eval(Tape, {write, X}) ->
    write(Tape, X).


%% eval_list/2: apply to the input tape a list of commands, and return
%%              the resulting tape
%%
%% The command list must be populated with the same commands available to
%% the eval/1 command. If the list is empty, the input list is returned
%% unchanged.

-spec eval_list(tape(), [command()]) -> tape().
eval_list(Tape, []) ->
    Tape;
eval_list(Tape, [Cmd|Rest]) ->
    T2 = eval(Tape, Cmd),
    eval_list(T2, Rest).


%% to_list/1: returns a list representation of the tape
%%
%% Since the tape could be potentially infinite, the returned list
%% cuts off all the left and right tails of empty cell, so the list
%% starts from the leftmost non-empty cell up to the rightmost non-empty
%% cell (unless the cursor has moved left or right into the "void", in
%% this case empty cells are in the tails to keep track of the position.)
%% The cursor position is lost, so the resulting list cannot be turned
%% back to the tape recovering the cursor position.

-spec to_list(tape()) -> [cell()].
to_list({X, Left, Right}) ->
    lists:reverse(Left) ++ [X] ++ Right.

-spec to_string(tape()) -> string().
to_string({Cur, Left, Right}) ->
    StrL = cell_list_to_str(lists:reverse(Left)),
    StrCur = cell_to_str(Cur),
    StrR = cell_list_to_str(Right),
    StrL ++ "[" ++ StrCur ++ "]" ++ StrR.

-spec cell_to_str(cell()) -> string().
cell_to_str({}) ->
    " ";
cell_to_str(X) when is_list(X) ->
    X;
cell_to_str(X) when is_integer(X) ->
    integer_to_list(X).

-spec cell_list_to_str([cell()]) -> string().
cell_list_to_str(L) ->
    cell_list_to_str(L, "|").

-spec cell_list_to_str([cell()], string()) -> string().
cell_list_to_str(L, Sep) ->
    lists:concat(
      lists:join(Sep, lists:map(fun cell_to_str/1, L))).
