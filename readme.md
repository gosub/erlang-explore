# Erlang Explore

## Modules


### turing_tape

Implements a data structure to simulate a tape, infinite in both directions. The tape is subdivided in cells, that can be empty or populated with a symbol. Used in the **turing_machine** module. The tape API is the set of possible operations defined by Alan Turing in its paper "*On Computable Numbers, With An Application To The Entscheidungsproblem*":

- *left*: shift the tape left by one cell
- *right*: shift the tape right by one cell
- *clear*: empty the cell under the cursor
- *{write, X}*: write the symbol X in the cell under the cursor
- *read*: return the symbol under the cursor

The underlying implementation for the tape is a [zipper data structure](https://en.wikipedia.org/wiki/Zipper_\(data_structure\)).


### turing_machine

A module to define and run Turing Machines. Each machine is made of the current state symbol, a tape, and the table that maps the current state and the symbol under the cursor of the tape to the next state and the operations to perform on the tape.

### turing_machines

A collection of turing machines definitions, mostly reimplemented from *The Annotated Turing* by Charles Petzold.
These are the machines implemented until now:

- alternating_example: prints 1 and 0 alternating
- simplified_alternating_example: same machine as before, with a simpler definition
- runs_of_ones: prints runs of ones, separated by single 0s, each run is one longer than the previous
- sqrt_of_two: prints the binary representation of the fractional part of the square root of two

### mini_repl

A module to create small REPLs.

## Dev checklist

- [ ] code
- [ ] types
- [ ] readme desc
- [ ] comments
- [ ] tests?

## License

All the code inside this repository is released under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.html).

## Author

Giampaolo Guiducci <giampaolo.guiducci@gmail.com>