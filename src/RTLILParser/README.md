# About

This directory contains the sources for the Register Transfer Logic
Intermediate Language(RTLIL) used by Yosys. RTLIL started off as an
internal language within the Yosys synthesis engine, but later, 
an official Yosys RTLIL language frontend(ingester) and backend(emitter)
emerged along with an accompanying RTLIL EBNF grammar. Included in this
directory is the RTLIL EBNF grammar that was referenced when constructing
the Haskellator RTLIL Parsec parser contained in the directory.

Of note is that there may be some deviations in the behavior of the
Haskellator Parser implementation from the actual Lex/Yacc implementation
used in the Yosys frontend. These deviations arise because the Lex/Yacc implementation in the Yosys frontend deviates from the EBNF RTLIL 
grammar. I make an attempt to capture these deviations in the 
"Discrepancies between Lex/Yacc Yosys RTLIL Frontend and Yosys 
Documentation EBNF Grammar" section in this README.

Lastly, a copy of the grammar that was referenced when building the
Haskellator RTLIL parser is included in this directory as 
"rtlil_text.rst". You can also find this document in the upstream
Yosys sources pinned to commit `8148ebd` [here][ebnf-yosys-upstream].

# Discrepancies between Lex/Yacc Yosys RTLIL Frontend and Yosys Documentation EBNF Grammar
1. As of Yosys commit `8148ebd`, the Lex/Yacc RTLIL frontend allows
   attribute statements, switch statements, and assignment statements
   to appear in any order at the root level of a process body.
   The relevant snippet of Yacc code can be found
   [here][yacc-code-snippet].

   By contrast, the EBNF grammar doc as of commit `8148ebd` allows
   multiple switch statements at the root level of a process body,
   but requires that all assignment statements occur before the 
   first switch statement. The EBNF grammar also effectively 
   requires that attribute statements be placed above their respectve
   switch statement. In practice, this second deviation is not an
   issue as I've never seen a tool that emits RTLIL violate it.
   The revelant snippet of the EBNF grammar can be found
   [here][ebnf-grammar-snippet].

[ebnf-yosys-upstream]: https://github.com/YosysHQ/yosys/blob/87736a2bf9710e307fbf9e57e6cece7586314cf7/docs/source/appendix/rtlil_text.rst
[yacc-code-snippet]: https://github.com/YosysHQ/yosys/blob/87736a2bf9710e307fbf9e57e6cece7586314cf7/frontends/rtlil/rtlil_parser.y#L337-L341
[ebnf-grammar-snippet]: https://github.com/YosysHQ/yosys/blob/87736a2bf9710e307fbf9e57e6cece7586314cf7/docs/source/appendix/rtlil_text.rst?plain=1#L253