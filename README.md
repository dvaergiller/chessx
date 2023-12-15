# ChessX - HTMX experiment/showcase

Pure HATEOAS based chess application made to try out HTMX feature set.

# To run

You need haskell compiler GHC and build system Cabal:

## Build
```
$ cabal build
```

## Run

```
$ cabal run
```

# TODO

- Detect check/checkmate
- Disallow moves placing own player in check
- Castling (all game info is available in board to implement logic)
- En passant (all game info is available in board to implement logic)
- Player info
