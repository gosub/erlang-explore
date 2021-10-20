-module(turing_machines).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").

-export([make/1, make/2]).
-export([alternating_example/0, simplified_alternating_example/0]).

make(Machine) ->
    make(Machine, "𝔟").

make(Machine, BeginState) ->
    turing_machine:new(Machine, BeginState).


alternating_example() ->
[
 "𝔟", {}, [{write, 0}, right], "𝔠",
 "𝔠", {}, [right],             "𝔢",
 "𝔢", {}, [{write, 1}, right], "𝔨",
 "𝔨", {}, [right],             "𝔟"
].

simplified_alternating_example() ->
[
 "𝔟", {}, [{write, 0}],               "𝔟",
 "𝔟", 0,  [right, right, {write, 1}], "𝔟",
 "𝔟", 1,  [right, right, {write, 0}], "𝔟"
].
