# fib

```text
fib v1.1.2

USAGE:
  fib
  fib <N>...
  fib --until N

Calculate members of the Fibonacci sequence.

Every N provided as an argument will return a line with the Nth member
of the Fibonacci sequence. When no arguments are provided, the program
instead reads lines from standard input. Arguments (or lines from
standard input) should be non-negative integer values.

This CLI considers a 0-indexed sequence, where the 0th number is 0,
the 1st is 1, then 1, 2, 3, 5, 8...

When '--until N' is used, all members of the sequence are printed on
N+1 consecutive lines, including and ending with the Nth number of the
sequence.

This project lives at https://github.com/hiljusti/fib
```

## Requirements

- [GHC](https://www.haskell.org/downloads/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Notes

- Uses [`gutenberg-fibonaccis`](https://hackage.haskell.org/package/gutenberg-fibonaccis)
  for the first 1001 numbers. (Not including zero)
- Uses [`fibonacci`](https://hackage.haskell.org/package/fibonacci) when
  calculating larger fibonacci numbers.
- Uses the classic `zipWith` technique when calculating large ranges of numbers.

## Installation through Stack

```bash
cd $(mktemp -d) # Or wherever you prefer
git clone https://github.com/hiljusti/fib.git
cd ./fib
stack install
```
