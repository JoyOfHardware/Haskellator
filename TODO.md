 # General and Planning
 - [ ] might need many validation phases
   - [ ] it's conceivable that one could construct a Yosys memory Cell 
         with invalid parameters
   - [ ] detect mismatched sizes in assignments
 - [ ] need canonical form making it easy to run all these validation 
       passes
 - [ ] are recursive slices inclusive?
 - [ ] what are the semantics of connection `<conn-stmt>`? Where is 
       `<conn-stmt>` employed?
 - [ ] Validate that `123456789[0:9][0:8]` is valid RTLIL
 - [ ] add validation section to README
 - [ ] just support Cell memV2
 - [ ] for value, what happens if we have 0 binary digits
 - [ ] modify parser to ignore comments... - should probably wait until 
       parser is finished before asking
 - [ ] modify AST to support src tracking - needed to allow for human 
       readable and correctable validation errors
 - [ ] when writing simulator, must specify directions on cell ports
 - [ ] in the <process>, why are we allowed to have <assign-stmt> before
       and after the optional <switch> stmt?
 - [ ] inspect Chris's mini-RTLIL
 - [ ] add RST grammar file to repo
 - [ ] name parsers so that that we know where failures occured
 - [ ] may want to also derive equality statements
 - [x] replace both `pEol *> pMaybeWs` and `pEol <* pMaybeWs` 
       with `pEolAndAdvanceToNextNonWs`

 # Parser Development
  - [ ] Sync
  - [ ] Process
  - [ ] Module

 # Parser Verification
 - [ ] I think only EOL terminated parsers should be responsible 
       for pre-winding the Parsec scanner to the next non-space...
 - [ ] lift grammar into prover and show that all EOL terminated parsers
       are either followed by EOF or a keyword such "module", "autoidx",
       etc
 - [ ] first, manually inspect switch parser and try and see if it
       can infinitely recurse... Then empirically validate against
       corpus

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