# What is it?

Haskellator is an experimental tool designed to unlock new possibilities in processing RTL (Register-Transfer Level) netlists. It aims to enhance simulation efficiency, offer insightful change analysis, and explore experimental synthesis techniques.

### Key Features:

- **Sparse Simulation**:
  Traditional open-source RTL simulators store signal values every time a signal changes. 
  HaskellatorSim will save signal states only if they’ve changed 
  within the last host CPU second of simulation. This approach compresses the simulation 
  data whilst enabling quick recomputation of any gaps when viewing saved simulations. 

  This requires access to the RTLIL sources and simulation save files to fill in any gaps.

- **Change Condition Analysis**:
  Haskellator will provide tools to analyze and trace the cause of signal changes during 
  simulation. By forming hypotheses about which signals influenced a change, it offers a 
  "git-blame" style feature for simulations. This functionality also requires access to 
  the original RTLIL sources and simulation save files.

- **Experimental Synthesis Techniques**:
  We hope to explore the potential of using reinforcement learning neural networks to 
  optimize synthesis passes for input netlists. The synthesized netlist could be placed 
  and routed to determine fmax, which could be used as the score the neural network will
  learn to optimize.

These are just a few of the concepts we're experimenting with. The broader goal is to 
explore innovative ideas, develop a high-quality tool that can evolve with community 
input, and just have fun!

# Status

Right now Haskellator can successfully parse some valid RTLIL into [an AST](./src/RTLIL/Syntax.hs).
We are currently working on expanding the RTLIL that can be parsed to include
[RTLIL](https://github.com/YosysHQ/yosys/blob/main/kernel/rtlil.h) 
emitted from Amaranth Lang.

# Usage

## Run and Build With Nix(Linux and MacOS)

The following will allow you to see a pretty printed
AST for the given input `il` file.

```bash
$ nix-shell
$ rtlil-parse test/corpus/xprop_dffe_1nnd_wrapped_xprop.il -o parsed1.ast
```

## Run With Stack

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

## Build With Stack

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
