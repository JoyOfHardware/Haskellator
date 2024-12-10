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
       and after the optional <switch> stmt? If the ordering here is
       semantically significant, then modify AST to only have single
       `[AssignStmt]` field and update parser behavior accordingly.
 - [ ] inspect Chris's mini-RTLIL
 - [ ] add RST grammar file to repo
 - [ ] name parsers so that that we know where failures occured
 - [ ] may want to also derive equality statements
 - [x] replace both `pEol *> pMaybeWs` and `pEol <* pMaybeWs` 
       with `pEolAndAdvanceToNextNonWs`
 - [x] Check inline sequencing of whitespace parsers in do blocks.
       Terminating instances of `pWs` should be preceeded by `<*`
 - [ ] Verify no backtracking needed when sequencing `many` parsers.
       Basically, we want to make sure that the argument of the `many`
       parser doesn't conflict(exhibit a partial early match) with 
       the argument of the parser after the argument of the `many` parser.

# Parser Development
 - [x] Sync
 - [ ] Process
 - [x] Finish `pCell` with `pCellEndStmt`
 - [ ] Rewrite `pWireStmt` and `pMemoryStmt` using do-notation...
 - [x] Remove all instances of `_ <-`
 - [ ] Module
 - [ ] Remove weird GHC imports
 - [ ] Embed locs in AST
 - [ ] Scrap `pEolAndAdvanceToNextNonWs` and use `tok`
 - [ ] Remove `preProcessDiscardComments` from exports
 - [ ] Install README in dir containing `Parser.hs`
 - [ ] Discuss deviations of parser against Yosys behaviors
 - [x] Are the `try` statements in `pWireOption` correctly constructed?
 - [ ] Consider the very weird case where the process body has nothing,
       thus, `pEolAndAdvanceToNextNonWs` may never get invoked in any of
       the sub-parsers encapsulated in `pProcessBody`. Do we need to 
       advance whitespaces so we can hit `<proc-end-stmt>`?
      - [ ] What are the implications for other parsers?

            I think that in this case we're OK as `<proc-stmt>` necessarily
            precedes `<process-body>` and `<proc-stmt>` terminates in an EOL
            parser that advances to the next non-whitespace.

            I still need to verify how other parsers behave. For example, what
            happens if we have a cell with no `<cell-body-stmt>`

# Parser Verification
 - [ ] I think only EOL terminated parsers should be responsible 
       for advancing the Parsec scanner to the next non-space...
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