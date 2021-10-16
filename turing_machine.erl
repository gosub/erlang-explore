-module(turing_machine).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").

-export([new/2, new/3, step/1]).

new(Table, InitState) ->
    new(Table, InitState, turing_tape:new()).

new(Table, InitState, Tape) ->
    {Table, InitState, Tape}.

step({Table, State, Tape}) ->
    Read = turing_tape:read(Tape),
    {ok, {Actions, NewState}} = maps:find({State, Read}, Table),
    NewTape = turing_tape:eval_list(Tape, Actions),
    {Table, NewState, NewTape}.
