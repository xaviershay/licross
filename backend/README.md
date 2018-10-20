# Licross

Like Lichess but for crossword games.

Not a real thing.

## Development

    stack build
    stack ghci

### Style Guide

* Qualify most imports, except some really commons ones in `Licross.Prelude`
* Write lens out by hand rather than use template haskell. No particular
  reason, just want to try it to see how painful it is.
