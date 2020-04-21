# SchemeJVM

In case this is not clear, this is a toy language, obviously not suited for production use.

## Running the Scheme meta-circular evaluator

Build the bootstrap interpreter.

```shell
# cd bootstrap/
# gradle installDist
```

Run the bootstrap interpreter from the Scheme source directory.

```shell
# cd ../scm/
# ../bootstrap/
# ../bootstrap/build/install/schemejvm/bin/schemejvm
> (display "Hello World")
Hello World
#!void
```

Load the Scheme source, and start a nested REPL.

```shell
> (load "all.scm")
#!void
> (run-repl "#>")
#> (load "stdlib.scm")
#> (display "Hello World")
Hello World
#!void
```

Let's go one step deeper, and load all of the Scheme interpreter source _again_,
and start another nested REPL. (This will take in the order of 10 minutes depending
on the speed of your CPU: those macros don't expand themselves!)

```shell
#> (load "all.scm")
#> (run-repl "##>")
##> (begin
  (define (fac acc n)
    (if (> n 0)
      (fac (* acc n) (- n 1))
      acc))
  (fac 1 6))
720
```

Enjoy the (slow) meta silliness.
