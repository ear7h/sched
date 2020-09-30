# Sched

Sched is a "schedule combinators" library, which lets you combine scheduling constraints
and make queries agains it. Basically, there is a defined `Data.Sched` monadic type
which is similar to `Maybe` but it's validity is temporal.
See `examples/Simple.hs` for an example.

This was written as an exercice to become familiar with haskell and monads.

## Todo

* documentation
* more test cases
* algorithms for finding the longest valid time in a `Sched` (within some bounds)
