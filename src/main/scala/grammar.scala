// repl ::= statement*

// statement   ::= expression ";"
//               | assignment
//               | conditional
//               | loop
//               | block

// assignment  ::= ident "=" expression ";"
// conditional ::= "if" "(" expression ")" block [ "else" block ]
// loop        ::= "while" "(" expression ")" block
// block       ::= "{" statement* "}"

// expression ::= term { ("+" | "-") term }*
// term       ::= factor { ("*" | "/" | "%") factor }*
// factor     ::= number
//              | ident
//              | "(" expression ")"

// ident      ::= letter { letter | digit | "_" }*
// number     ::= digit { digit }*

// letter     ::= "a" | ... | "z" | "A" | ... | "Z"
// digit      ::= "0" | ... | "9"

