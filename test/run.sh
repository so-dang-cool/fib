#!/bin/bash

# We assume this is being run by 'stack test' from the package root directory.
exec ./.stack-work/dist/*/*/build/fib/fib "$@"
