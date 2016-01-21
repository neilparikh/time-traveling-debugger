# time-traveling-debugger

A time traveling debugger written in Haskell, that allows for the user to step both forwards, and backwards in a program.

The ability to step backwards is implemented through a stack of states. Stepping forwards pushes a new state onto the stack, while stepping backwards pops a state from the stack.
