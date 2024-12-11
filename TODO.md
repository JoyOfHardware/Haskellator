# General and Planning
 - [ ] might need many validation phases
   - [ ] it's conceivable that one could construct a Yosys memory Cell 
         with invalid parameters
   - [ ] detect mismatched sizes in assignments
 - [ ] need canonical form making it easy to run all these validation 
       passes
 - [ ] are recursive slices inclusive?
 - [ ] you can apparently have empty SigConCats - but what does this
       even mean?
 - [ ] what are the semantics of connection `<conn-stmt>`?
 - [ ] Validate that `123456789[0:9][0:8]` is valid RTLIL
 - [ ] add validation section to README
 - [ ] just support Cell memV2
 - [ ] for value, what happens if we have 0 binary digits
 - [ ] modify AST to support src tracking - needed to allow for human 
       readable and correctable validation errors
 - [ ] when writing simulator, must specify directions on cell ports
 - [ ] track RTLIL EBNF spec vs RTLIL Lex/Yacc implementation
       [deviation issue][deviation-issue]
 - [ ] Update issue above with fact that `<memwr>` is also missing
 - [ ] may want to also derive equality statements
 - [x] Check inline sequencing of whitespace parsers in do blocks.
       Terminating instances of `pWs` should be preceeded by `<*`

# Parser Development
 - [ ] Remove weird GHC imports
 - [ ] Test `fsm1.il` which has recursive slices on line 180
 - [ ] Embed locs in AST
 - [ ] Name new parsers
 - [ ] Remove `a` and `val` from exports
 - [ ] Permit all binary characters. Add validation pass later.
 - [ ] Correct comment parser
 - [ ] What does it mean to have an empty sigspec concat in
       `{ <sigspec>* }`

# Parser Verification
 - [ ] I think only EOL terminated parsers should be responsible 
       for advancing the Parsec scanner to the next token
 - [ ] lift grammar into prover and show that all EOL terminated parsers
       are either followed by EOF or a keyword such "module", "autoidx",
       etc

# Simulation Behavior
 - [ ] Figure out the computational semantics of what it means to
       sync on `high`. I already understand the computational
       semantics around synchronizing on `posedge` for example...

# Simulation
 - [ ] dump to VCD
 - [ ] Write dynamically typed executor in Haskell
 - [ ] Subsequent IR passes may emerge natrually

# Optimizations
 - [ ] squeeze out recursive slices

[deviation-issue]: https://github.com/YosysHQ/yosys/issues/4811