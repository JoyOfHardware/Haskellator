 # General and Planning
 - [ ] might need many validation phases
   - [ ] it's conceivable that one could construct a Yosys memory Cell with invalid parameters
   - [ ] detect mismatched sizes in assignments
 - [ ] need canonical form making it easy to run all these validation passes
 - [ ] are recursive slices inclusive
 - [ ] what are the semantics of connection `<conn-stmt>`? Where is `<conn-stmt>` employed?
 - [ ] Validate that `123456789[0:9][0:8]` is valid RTLIL
 - [ ] reload VSCode window
 - [ ] add validation section to README
 - [ ] just support Cell memV2
 - [ ] for value, what happens if we have 0 binary digits
 - [ ] ask ChatGPT about how to get my parser to ignore comments... - should probably wait until parser is finished before asking
 - [ ] modify AST to support src tracking - needed to allow for human readable and correctable validation errors
 - [ ] you could have a concated signal that gets sliced
 - [ ] when writing simulator, must specify directions on cell ports
 - [ ] in the <process>, why are we allowed to have <assign-stmt> before and after the optional <switch> stmt?
 - [ ] make TODO file
 - [ ] inspect Chris's mini-RTLIL
 - [ ] split/organize imports/exports by section
 - [ ] add RST grammar file to repo
 - [ ] figure out whitespaces
 - [ ] I think only EOL terminated parsers should be responsible for pre-winding the Parsec scanner to the next non-space...
 - [ ] lift grammar into prover and show that all EOL terminated parsers are either followed by EOF or a keyword such "module", "autoidx", etc
 - [ ] name parsers that that we know where failures occured?
 - [ ] first, manually inspect switch parser and try and see if it
   can infinitely recurse... Then empirically validate against corpus

 # Simulation
 - [ ] dump to VCD
 - [ ] Write dynamically typed executor in Haskell
 - [ ] Subsequent IR passes may emerge natrually

# Optimizations
 - [ ] squeeze out recursive slices