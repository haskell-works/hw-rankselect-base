# hw-rankselect-base
[![CircleCI](https://circleci.com/gh/haskell-works/hw-rankselect-base.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-rankselect-base)

Rank and select operations.

This library will use support for some BMI2 CPU instructions on some x86 based
CPUs if compiled with the appropriate flags on `ghc-8.4.1` or later.

## Rank and select

This library provides the following functions on various types:

* `rank1`
* `rank0`
* `select1`
* `select0`

Type class instances are provided for the following primitive types:

* `Bool`
* `Word8`
* `Word16`
* `Word32`
* `Word64`

Moreover additional type class instances are provided for `[]`, `Vector`
from both `Data.Vector`, and `Data.Vector.Storable` of these primitive
types.

The collection-based type classes instances are not intended to be used
in high-performance code because they have poor performance for most
purposes and are provided purely as reference implementations.

Bit-vectors larger than 64-bits need indexing to achieve higher performance.
A indexed bit-vector implementation can found in the
[hw-rankselect](https://hackage.haskell.org/package/hw-rankselect) package.

## Notes

This library follows standard 1-based counting conventions typically found in
Computer Science literature where `select1 10 2 = 4` as illustrated here:

```text
  8 7 6 5  [4]3 2 1
  0 0 0 0   1 0 1 0
```

The standard convention for the `bmi2` implementation, comes at a small cost.

An internal function `select1Word64Bmi2Base0` demonstrates 0-based counting
that is slightly faster when implemented with the `bmi2` instruction set where
`select1 10 1 = 3` as illustrated here:

```text
  7 6 5 4  [3]2 1 0
  0 0 0 0   1 0 1 0
```

## Compilation

It is sufficient to build, test and benchmark the library as follows
for emulated behaviour:

```text
stack build
stack test
stack bench
```

To target the BMI2 instruction set, add the `bmi2` flag:

```text
stack build --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2
stack test  --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2
stack bench --flag bits-extra:bmi2 --flag hw-rankselect-base:bmi2
```

## Benchmark results

The following benchmark shows the kinds of performance gain that can
be expected from enabling the BMI2 instruction set for CPU targets
that support them:

```text
benchmarking 64-bit/Once: Select1 Broadword
time                 14.75 ns   (14.63 ns .. 14.90 ns)
                     0.996 R²   (0.987 R² .. 0.999 R²)
mean                 15.35 ns   (14.92 ns .. 16.70 ns)
std dev              2.355 ns   (607.2 ps .. 4.849 ns)
variance introduced by outliers: 96% (severely inflated)

benchmarking 64-bit/Once: Select1 Bmi2
time                 6.026 ns   (5.933 ns .. 6.134 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 6.024 ns   (5.966 ns .. 6.096 ns)
std dev              224.4 ps   (176.9 ps .. 318.6 ps)
variance introduced by outliers: 62% (severely inflated)

benchmarking 32-bit/Once: Select1 Broadword
time                 26.09 ns   (25.84 ns .. 26.40 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 26.67 ns   (26.37 ns .. 27.01 ns)
std dev              1.017 ns   (848.4 ps .. 1.291 ns)
variance introduced by outliers: 61% (severely inflated)

benchmarking 32-bit/Once: Select1 Bmi2
time                 8.613 ns   (8.543 ns .. 8.687 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.592 ns   (8.515 ns .. 8.671 ns)
std dev              248.3 ps   (216.2 ps .. 294.8 ps)
variance introduced by outliers: 48% (moderately inflated)
```
