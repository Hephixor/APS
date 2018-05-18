open Ast
let rec print_prolog_expr e =
  match e with
  ASTNum n -> Printf.printf"%d" n
  | ASTId x -> Printf.printf "%s" x
  | ASTBool x -> Printf.printf"\"%b\"" x
  | ASTUnary(op, e) -> (
    Printf.printf"%s" (string_of_op op);
    Printf.printf"( ";
    print_prolog_expr e;
    Printf.printf") "
  )
  | ASTPrim(op, e1, e2) -> (
    Printf.printf"%s" (string_of_op op);
    Printf.printf"( ";
    print_prolog_expr e1;
    Printf.printf", ";
    print_prolog_expr e2;
    Printf.printf") "
  )
  | ASTApp(e1,e2) -> (
    Printf.printf"app(";
    print_prolog_expr e1;
    Printf.printf ",";
    print_prolog_exprs e2;
    Printf.printf") "
  )
  | ASTIf(e1,e2,e3) -> (
    Printf.printf"if";
    Printf.printf "( ";
    print_prolog_expr e1;
    Printf.printf ", ";
    print_prolog_expr e2;
    Printf.printf ", ";
    print_prolog_expr e3;
    Printf.printf ") "
  )
  | ASTAbstr (ar,ex) -> (
   Printf.printf "abstr([ ";
   print_prolog_args ar;
   Printf.printf " ],";
   print_prolog_expr ex;
   Printf.printf") "
 )
  and print_prolog_dec d =
  match d with
  ASTConst(ide, ty, x) -> (
    Printf.printf"const(%s" ide;
    Printf.printf ", ";
    print_prolog_typ ty;
    Printf.printf ", ";
    print_prolog_expr x;
    Printf.printf ") ";
  )
  | ASTFun(ide,ty,args,expr) -> (
    Printf.printf"fun(%s, [" ide;
    print_prolog_args args;
    Printf.printf"], ";
    print_prolog_typ ty;
    Printf.printf ", ";
    print_prolog_expr expr;
    Printf.printf ") "
  )
  | ASTFRec(ide,ty,args,expr) -> (
    Printf.printf"funrec(%s, [" ide;
    print_prolog_args args;
    Printf.printf"], ";
    print_prolog_typ ty;
    Printf.printf ", ";
    print_prolog_expr expr;
    Printf.printf ") "
  )
  | ASTVar(ide, ty) -> (
    Printf.printf"var( %s, " ide;
    print_prolog_typ ty;
    Printf.printf ") "
  )
  | ASTProc(ide, args, pro) -> (
    Printf.printf"proc(%s, [" ide;
    print_prolog_args args;
    Printf.printf"], ";
    print_prolog_prog pro;
    Printf.printf " ) "
  )
  | ASTProRec(ide, args, pro) -> (
    Printf.printf"procrec(%s, [" ide;
    print_prolog_args args;
    Printf.printf"], ";
    print_prolog_prog pro;
    Printf.printf " ) "
  )
  and print_prolog_exprs es =
  match es with
  | ASTExpres e -> print_prolog_expr e
  | ASTExprs(e, e') ->(
    print_prolog_expr e;
    print_prolog_exprs e'
  )
  and print_prolog_arg s =
  match s with
  ASTArgument(s,t) ->(
    Printf.printf " (%s," s;
    print_prolog_typ t;
    Printf.printf") "
  )
  and print_prolog_args ags =
  match ags with
  ASTArg(s) -> print_prolog_arg s
  | ASTArgs(s, s') -> (
    print_prolog_arg s;
    Printf.printf ", ";
    print_prolog_args s'
  )
  and print_prolog_typ t =
  match t with
  ASTTyprim t ->(
    Printf.printf "%s" (string_of_tprim t)
  )
  | ASTTyfun(ts,t) -> (
    Printf.printf" fleche([";
    print_prolog_typs ts;
    Printf.printf"], ";
    print_prolog_typ t;
    Printf.printf") "
  )
  and print_prolog_typs ts =
  match ts with
  [] -> ()
  |t::[] -> print_prolog_typ t
  | t::ts -> (
    print_prolog_typ t;
    Printf.printf", ";
    print_prolog_typs ts
  )
  and print_prolog_stat p =
  match p with
  | ASTEcho p -> (
    Printf.printf "echo( ";
    print_prolog_expr p;
    Printf.printf") "
  )
  | ASTSet (ide, exp) -> (
    Printf.printf "set( %s, " ide;
    print_prolog_expr exp;
    Printf.printf ") "
  )
  | ASTCall (ide, exprs) -> (
    Printf.printf "call( %s, " ide;
    print_prolog_exprs exprs;
    Printf.printf ") "
  )
  | ASTIFSt (exp, prth, prels) ->(
    Printf.printf "ifst( ";
    print_prolog_expr exp;
    Printf.printf ", ";
    print_prolog_prog prth;
    Printf.printf ", ";
    print_prolog_prog prels;
    Printf.printf ") "
  )
  | ASTWhile (exp, pro) -> (
    Printf.printf "while( " ;
    print_prolog_expr exp;
    Printf.printf ", ";
    print_prolog_prog pro;
    Printf.printf ") "
  )
  and print_prolog_cmds cmd =
  match cmd with
  |ASTStat cmd ->(
    print_prolog_stat cmd;
  )
  |ASTStatCmds (c,m) ->(
    print_prolog_stat c;
    Printf.printf" , ";
  print_prolog_cmds m)
  |ASTDecCmds (cmd,a) -> (
    print_prolog_dec cmd;
    Printf.printf" , ";
    print_prolog_cmds a
  )
  and print_prolog_prog ppp =
  match ppp with
  ASTProg ppp -> (
    Printf.printf"[";
    print_prolog_cmds ppp;
    Printf.printf" ]";
  )

let _ =
try
let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
let e = Parser.prog Lexer.token lexbuf in
print_prolog_prog e;
print_char '\n'
with Lexer.Eof -> exit 0