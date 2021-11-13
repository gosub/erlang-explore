-module(turing_machines).
-author("Giampaolo Guiducci <giampaolo.guiducci@gmail.com>").
-license("GNU GPL v3.0").

-export([make/1, make/2]).
-export([alternating_example/0, simplified_alternating_example/0]).
-export([runs_of_ones/0, positive_integers/0, sqrt_of_two/0]).


-spec make(turing_machine:rule_list()) -> turing_machine:turing_machine().
make(Machine) ->
    make(Machine, "𝔟").


-spec make(turing_machine:rule_list(), turing_machine:state()) -> turing_machine:turing_machine().
make(Machine, BeginState) ->
    turing_machine:new(Machine, BeginState).


-spec alternating_example() -> turing_machine:rule_list().
alternating_example() ->
[
 "𝔟", {}, [{write, 0}, right], "𝔠",
 "𝔠", {}, [right],             "𝔢",
 "𝔢", {}, [{write, 1}, right], "𝔨",
 "𝔨", {}, [right],             "𝔟"
].


-spec simplified_alternating_example() -> turing_machine:rule_list().
simplified_alternating_example() ->
[
 "𝔟", {}, [{write, 0}],               "𝔟",
 "𝔟", 0,  [right, right, {write, 1}], "𝔟",
 "𝔟", 1,  [right, right, {write, 0}], "𝔟"
].


-spec runs_of_ones() -> turing_machine:rule_list().
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

sqrt_of_two() ->
[
 begin_, {}, [{write, "@"}, right, {write, 1}], new,

 new, "@", [right], mark_digits,
 new, any, [left], new,

 mark_digits, 0, [right, {write, "x"}, right], mark_digits,
 mark_digits, 1, [right, {write, "x"}, right], mark_digits,
 mark_digits, {}, [right, {write, "z"}, right, right, {write, "r"}], find_x,

 find_x, "x", [clear], first_r,
 find_x, "@", [], find_digits,
 find_x, any, [left, left], find_x,

 first_r, "r", [right, right], last_r,
 first_r, any, [right, right], first_r,

 last_r, "r", [right, right], last_r,
 last_r, {},  [{write, "r"}, right, right, {write, "r"}], find_x,

 find_digits, "@", [right, right], find_1st_digit,
 find_digits, any, [left, left], find_digits,

 find_1st_digit, "x", [left], found_1st_digit,
 find_1st_digit, "y", [left], found_1st_digit,
 find_1st_digit, "z", [left], found_2nd_digit,
 find_1st_digit, {},  [right, right], find_1st_digit,

 found_1st_digit, 0, [right], add_zero,
 found_1st_digit, 1, [right, right, right], find_2nd_digit,

 find_2nd_digit, "x", [left], found_2nd_digit,
 find_2nd_digit, "y", [left], found_2nd_digit,
 find_2nd_digit,  {}, [right, right], find_2nd_digit,

 found_2nd_digit, 0,  [right], add_zero,
 found_2nd_digit, 1,  [right], add_one,
 found_2nd_digit, {}, [right], add_one,

 add_zero, "r", [{write, "s"}], add_finished,
 add_zero, "u", [{write, "v"}], add_finished,
 add_zero, any, [right, right], add_zero,

 add_one, "r", [{write, "v"}], add_finished,
 add_one, "u", [{write, "s"}, right, right], carry,
 add_one, any, [right, right], add_one,

 carry, "r", [{write, "u"}], add_finished,
 carry, {}, [{write, "u"}], new_digit_is_zero,
 carry, "u", [{write, "u"}, right, right], carry,

 add_finished, "@", [right, right], erase_old_x,
 add_finished, any, [left, left], add_finished,

 erase_old_x, "x", [clear, left, left], print_new_x,
 erase_old_x, "z", [{write, "y"}, left, left], print_new_x,
 erase_old_x, any, [right, right], erase_old_x,

 print_new_x, "@", [right, right], erase_old_y,
 print_new_x, "y", [{write, "z"}], find_digits,
 print_new_x, {},  [{write, "x"}], find_digits,

 erase_old_y, "y", [clear, left, left], print_new_y,
 erase_old_y, any, [right, right], erase_old_y,

 print_new_y, "@", [right], new_digit_is_one,
 print_new_y, any,  [{write, "y"}, right], reset_new_x,

 reset_new_x, {},  [right, {write, "x"}], flag_result_digits,
 reset_new_x, any, [right, right], reset_new_x,

 flag_result_digits, "s", [{write, "t"}, right, right], unflag_result_digits,
 flag_result_digits, "v", [{write, "w"}, right, right], unflag_result_digits,
 flag_result_digits, any, [right, right], flag_result_digits,

 unflag_result_digits, "s", [{write, "r"}, right, right], unflag_result_digits,
 unflag_result_digits, "v", [{write, "u"}, right, right], unflag_result_digits,
 unflag_result_digits, any, [], find_digits,

 new_digit_is_zero, "@", [right], print_zero_digit,
 new_digit_is_zero, any, [left], new_digit_is_zero,

 print_zero_digit, 0,  [right, clear, right], print_zero_digit,
 print_zero_digit, 1,  [right, clear, right], print_zero_digit,
 print_zero_digit, {}, [{write, 0}, right, right, right], cleanup,

 new_digit_is_one, "@", [right], print_one_digit,
 new_digit_is_one, any, [left], new_digit_is_one,

 print_one_digit, 0,  [right, clear, right], print_one_digit,
 print_one_digit, 1,  [right, clear, right], print_one_digit,
 print_one_digit, {}, [{write, 1}, right, right, right], cleanup,

 cleanup, {}, [], new,
 cleanup, any, [clear, right, right], cleanup
].
