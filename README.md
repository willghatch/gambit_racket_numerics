# Gambit Scheme numerics in Racket

Run Gambit Scheme numerics (gambit/lib/_num.scm) in Racket.

## Prerequisites

Racket (>6.3.0.14)
Gambit Scheme (gsi)

## Usage

Run racket numerics test suite with `racket number.rkt`.

Run bignum test suite with `racket bignum_test.rkt path/to/gsi`. The values in bignum_config.rkt must be set according to the gsi instance.

Generate _num.no#.scm, _num.rkt.orig, and _num.rkt.comment based on _num.scm with `generate.sh`.

## Structure

### _num.rkt

Partially commented out version of _num.scm. Defines numerical primitives.

### definitions.rkt

Definitions needed by _num.rkt.

### bignum.rkt

Definitions of bignum primitives.

### bignum_test.rkt

Tests the the bignum primitives implementation against gambit.

### toplevel.rkt

Packages primitives exported by _num.rkt as top-level functions, for example n-ary gambit-+ rather than binary @@+.

### translate.rkt

Provides functionality for transforming between racket's native representation of numbers and this project's representation.

### interface.rkt

Maps native racket functions to this project's functions for testing.

### testing.rkt

Uses the mappings in interface.rkt to provide a testing framework that tests using the project's implementations when available.

### number.rkt

Racket's numerics test suite. Uses the framework provided in testing.rkt.

## State of the project
