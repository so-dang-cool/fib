# fib

```text
fib v1.1

USAGE:
  fib
  fib <N>...
  fib --until N

Calculate members of the Fibonacci sequence.

Every N provided as an argument will return a line with the Nth member
of the Fibonacci sequence. When no arguments are provided, the program
instead reads lines from standard input. Arguments (or lines from
standard input) should be non-negative integer values.

When '--until N' is used, all members of the sequence are printed on
separate lines, finishing with the Nth member of the sequence.

This project lives at https://github.com/hiljusti/fib
```

## Requirements

- [GHC](https://www.haskell.org/downloads/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

## Installation

With Bash:

```bash
cd $(mktemp -d) \
  && git clone https://github.com/hiljusti/fib.git \
  && cd ./fib \
  && stack install
```

...or something similar for your shell.

