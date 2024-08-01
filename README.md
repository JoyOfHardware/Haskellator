# haskellator

Humble beginnings of what will likely become a tracing (supporting sparse simulation state dumps)
[RTLIL](https://github.com/YosysHQ/yosys/blob/main/kernel/rtlil.h) simulator integrated with an SMT
solver for retro-actively resolving trigger/change conditions for a given signal.

# Usage

# Run With Stack

Currently the main executable is the frontend to the RTLIL parser, called
`rtlil-parse`. Its only purpose is to test the parser.

This Haskell project can be built and tested using the [Haskell tool
Stack](https://docs.haskellstack.org/en/stable/), but generally straight
`cabal` should work as well (replace `stack` with `cabal` in the commands
below).

To build and run the project, use:
```
$ stack run
Usage: rtlil-parse [OPTION...] <filename>
  -v           --verbose  More verbose output.
  -h           --help     Print this message.
  -p           --pretty   Write pretty-printed output instead of AST (for testing).
  -o file.out             Output file name.
```

# Build With Stack

Alternatively, to build, install, and run the `rtlil-parse` executable, use:
```
$ stack install
$ rtlil-parse
Usage: rtlil-parse [OPTION...] <filename>
  -v           --verbose  More verbose output.
  -h           --help     Print this message.
  -p           --pretty   Write pretty-printed output instead of AST (for testing).
  -o file.out             Output file name.
```

The `rtlil-parse` executable accepts the path to an RTLIL file to parse (see
examples in `test/corpus`). It outputs the parsed AST to a `.out` file in the
same directory as the argument file. It can also output the pretty-printed
version of the parsed AST when given the `-p` flag, which should generally
match the original RTLIL except for whitespace.

We have a test suite of example RTLIL files in `test/corpus`. Each test in the
test suite compares the pretty-printed output of one of these files to the
original RTLIL and the test passes if they are the same (up to differences in
whitespace).  To run the test suite, use:
```
$ stack test
```

# Run and Build With Nix(Linux and MacOS)

The following will allow you to see a pretty printed
ast for the given input `il` file.

```bash
nix-shell
rtlil-parse test/corpus/xprop_dffe_1nnd_wrapped_xprop.il -o parsed1.ast
```
