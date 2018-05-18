{
open Parser (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
[' ' '\t' '\n'] { token lexbuf } (* skip blanks *)
(*| ['\n' ] { EOL }*)
| "true" { BOOLT}
| "false" {BOOLF}
| ['-']?['0'-'9']+('.'['0'-'9'])? as lxm { NUM(int_of_string lxm) }
| "if" as op { BINIF(op) }
| "add" as op { BINOP(op) }
| "sub" as op { BINOP(op) }
| "mul" as op { BINOP(op) }
| "div" as op { BINOP(op) }
| "not" as op { NOT(op) }
| "and" as op { BINOP(op) }
| "or" as op { BINOP(op) }
| "eq" as op { BINOP(op) }
| "lt" as op { BINOP(op) }
| "ECHO" { ECHO }
| "CONST" { CONST }
| "FUN" { FUN }
| "REC" { REC }
| "PROC" { PROC }
| "VAR" { VAR }
| "SET" { SET }
| "WHILE" { WHILE }
| "CALL" { CALL }
| "IF" { IF }
| "int" { INT }
| "bool" { BOOL }
| "void" { VOID }
| '*' { STAR }
| "->" { ARC }
| ';' { SEMICOLON }
| ':' { DDOTS }
| ',' { COMA }
| '[' { LCRO }
| ']' { RCRO }
| '(' { LPAR }
| ')' { RPAR }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z']* as id { IDENT(id) }
| eof { raise Eof }
