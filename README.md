# Licross

![Build Status](https://travis-ci.com/xaviershay/licross.svg?branch=master)

Like Lichess but for crossword games.

Not a real thing.

![Title Screen](https://raw.githubusercontent.com/xaviershay/licross/master/doc/title-screen.png)

## Development

    # First terminal
    cd backend/
    stack build
    stack test
    stack ghci -- run "main"

    # Second terminal
    cd frontend/
    bin/dev

### Style Guide

A few new style things I'm trying on this project. Don't have strong opinions,
just want to mix it up a bit.

* Qualify most imports, except some really commons ones in `Licross.Prelude`
* Write lens out by hand rather than use template haskell. No particular
  reason, just want to try it to see how painful it is.
* `hindent` everything.
* Label imports with source package.
