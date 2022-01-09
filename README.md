# fizzbuzz

[![CI](https://github.com/alessandrocandolini/fizzbuzz/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/fizzbuzz/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/fizzbuzz/branch/main/graph/badge.svg?token=LXFQ53067Y)](https://codecov.io/gh/alessandrocandolini/fizzbuzz)

Implementation of the fizzbuzz exercise, showcasing modelling with types etc

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

Assuming `stack` is installed in the system, the project can be build by running
```
stack build
```
To build and also run the tests, run
```
stack test
```
which is equivalent to
```
stack build --test
```

To run tests with coverage
```
stack test --coverage
```
which generates an html report. 

To run the executable,
```
stack exec fizzbuzz-exe
```
or
```
stack exec fizzbuzz-exe
```
(the latter is used when you have to pass arguments to the program)

For faster feedback loop,
```
stack test --fast --file-watch
```
To run `ghci` (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.
