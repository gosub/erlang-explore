-module(turing_machine).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").

-export([new/2, new/3]).

new(Table, InitState) ->
    new(Table, InitState, turing_tape:new()).

new(Table, InitState, Tape) ->
    {Table, InitState, Tape}.
