# Licross

Like Lichess but for crossword games.

Not a real thing.

## Development

    stack build
    stack test

    stack ghci
    > :set args ../templates/example.json
    > main

### Style Guide

A few new style things I'm trying on this project. Don't have strong opinions,
just want to mix it up a bit.

* Qualify most imports, except some really commons ones in `Licross.Prelude` or
  if import is entire point of file (e.g. `Json.hs`).
* Write lens out by hand rather than use template haskell. No particular
  reason, just want to try it to see how painful it is.
* `stylish-haskell` for imports.
  * Ideally would like to `hindent` everything, but currently doesn't work with
    `DerivingVia` (https://github.com/chrisdone/hindent/issues/523).
* Separate imports by source package.
