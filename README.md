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

Currently targetting RTLIL in Yosys .47.

# Usage

## Run and Build With Nix(Linux and MacOS)

The following will allow you to see a pretty printed
AST for the given input `il` file.

```bash
$ nix-shell
$ rtlil-parse
```

# TODO
 - [ ] automated CICD on gitea on personal servers
 - [ ] update to have support for four state logic by converting 'X' and 'Z' to zero
 - [ ] validation pass that checks that `ConstantInteger Int` is
   32 bits, that is, within range  \[-2147483648, 2147483648)
 - [ ] Reverse/repair cell-stmt

# Limitations
 - Does not support propagating non-two state logic, that is, no
   support for X or Z values. Default behavior is to reject such
   input although future iterations may support initializing X and
   Z to 0.
 - All cycles in circuit graphs must have at one D Flip-Flop on the
   cycle path. This requirement necesarily pre-cludes simulation of
   circuits such as NAND level-resolution SRAMs. The main reason for
   this restriction is to avoid having to handle metastability in 
   simulation.

   I have yet to evaluate the implications of how this affects
   multi-clock domain circuits and their associated primitives such
   as asynchronous FIFOs, but I plan to make sure simulation of such
   circuits is possible and correct.