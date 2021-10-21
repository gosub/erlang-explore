# Erlang Explore

## turing_tape

A module to simulate an infinite tape, in both direction, subdivided in cells where symbols can be written. Used in the turing_machine module. The tape api is made of the possible operations defined by Alan Turing in its paper "On Computable Numbers, With An Application To The Entscheidungsproblem" i.e.: left, right, write X, read. The tape is implemented as [zipper data structure](https://en.wikipedia.org/wiki/Zipper_\(data_structure\)).

## turing_machine

A module to define and run Turing Machines. Each machine is made of the current state symbol, a tape, and the table that maps the current state and the symbol under the cursor of the tape to the next state and the operations to perform on the tape.

## turing_machines

A collection of turing machines definitions, mostly reimplemented from *The Annotated Turing* by Charles Petzold.