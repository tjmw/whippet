# Whippet

A fuzzy file finder written in Haskell, inspired by [Greyhound](https://github.com/olivernn/greyhound).

### Usage

```
$ whippet <pattern> <dir>

e.g.

$ whippet "fbr" .
foobar.txt
```

### Development

Build locally into sandbox with cabal:

```
$ cabal sandbox init
$ cabal install -j
```