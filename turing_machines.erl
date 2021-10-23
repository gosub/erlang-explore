-module(turing_machines).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").

-export([make/1, make/2]).
-export([alternating_example/0, simplified_alternating_example/0]).
-export([runs_of_ones/0, positive_integers/0]).

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

runs_of_ones() ->
[
 "𝔟", {},  [{write, "ə"}, right, {write, "ə"},
            right, {write, 0}, right, right,
            {write, 0}, left, left],               "𝖔",

 "𝖔", 1,  [right, {write, "x"}, left, left, left], "𝖔",
 "𝖔", 0,  [],                                      "𝖖",

 "𝖖", any,[right, right],                          "𝖖",
 "𝖖", {}, [{write, 1}, left],                      "𝖕",

 "𝖕", "x",[clear, right],                          "𝖖",
 "𝖕", "ə",[right],                                 "𝖋",
 "𝖕", {}, [left, left],                            "𝖕",

 "𝖋", any,[right, right],                          "𝖋",
 "𝖋", {}, [{write, 0}, left,left],                 "𝖔"
].

positive_integers() ->
[
 begin_, {}, [{write, 0}], increment,
 increment, 0, [{write, 1}], rewind,
 increment, 1, [{write, 0}, left], increment,
 increment, {}, [{write, 1}], rewind,
 rewind, {}, [left], increment,
 rewind, any, [right], rewind
].
