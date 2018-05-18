open Ast

type env = (string * value_expr) list
and value_expr = Nat of int | Ferm of expr * string list * env | FermR of expr * string * string list * env | Void
and memory = ('a * int) list

let int_of_bool x =
	match x with
	|true -> 1
	|false -> 0

let bool_of_nat x =
	match x with
	|Nat(0) -> false
	|_ -> true


let rec ident_arg : Ast.args -> string list =
	fun arg -> match arg with 	
	|ASTArg (m) -> (match m with 
					|ASTArgument (s,_) -> s::[]
				)
	|ASTArgs (m,s) -> (match m with
					|ASTArgument (t,_) -> t::(ident_arg s)
				)

let rec ident_exprs exprs =
	match exprs with
	| ASTExpres (e) -> e::[]
	| ASTExprs (e,l)-> e::(ident_exprs l)

let rec find_env (env:env) e =
	match env with
	| [] -> failwith "id non present dans l'environnement"
	| (a,x)::_ when a = e -> x
	| (_,_)::l -> find_env l e


let add_memo env mem a =
	match env with
	|[] -> ()
	|_ -> ()


let rec eval_oprim_b env op e1 e2=
	match op with 
	Add -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat v1, Nat v2) -> Nat (v1 + v2)
		| _ -> failwith "c'est grave Add")
	| Mul -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat v1, Nat v2) -> Nat (v1 * v2)
		| _ -> failwith "c'est grave Mul") 
	| Sub -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat v1, Nat v2) -> Nat (v1 - v2)
		| _ -> failwith "c'est grave Sub")
	| Div -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat v1, Nat v2) -> Nat (v1 / v2)
		| _ -> failwith "c'est grave Div")
	| And -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat b1, Nat b2) -> Nat(b1*b2)
		| _ -> failwith "c'est grave And")
	| Or -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat b1, Nat b2) -> Nat(b1+b2)
		| _ -> failwith "c'est grave Or")
	| Eq -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat v1, Nat v2) -> Nat (int_of_bool(v1 == v2))
		| _ -> failwith "c'est grave Eq")
	| Lt -> (match (eval_expr env e1), (eval_expr env e2) with
		| (Nat v1, Nat v2) -> Nat (int_of_bool(v1 < v2))
		| _ -> failwith "c'est grave Lt")
	| _ -> failwith "None op"
and eval_oprim_u (env:env) op e =
	match op with
	Not -> (match (eval_expr env e) with
		| Nat b1 -> Nat (1 - b1)
		| _ -> failwith "c'est grave pour NOT")
	|True -> Nat 1
	|False -> Nat 0
	|_ -> failwith "None op"
and eval_expr (env:env) expr = 
	match expr with
	ASTNum(x) -> Nat x
	|ASTBool(b) -> if (b) then Nat(1) else Nat(0)
	|ASTId(x) -> (find_env env x)
	|ASTUnary(op,e) -> eval_oprim_u env op e 
	|ASTPrim(op,e1,e2) -> eval_oprim_b env op e1 e2
	|ASTIf(cond, th, el) -> if(bool_of_nat (eval_expr env cond))  then eval_expr env th else eval_expr env el
	|ASTAbstr (arg, exp) -> Ferm (exp, (ident_arg arg), env)
	|ASTApp(exp, exps) -> 
	(match eval_expr env exp with
		|Ferm (exa, idlist, envi) ->
		eval_expr ((List.map2 (fun x1 x2 -> (x1, x2)) 
						idlist 
						(List.map (fun x -> eval_expr envi x) (ident_exprs exps))
					)@env
				  ) exa
		|FermR (exa, lf, idlist, envi) ->
		eval_expr ((lf, FermR(exa,lf,idlist,envi))
				  ::
				  (List.map2 (fun x1 x2 -> (x1, x2)) 
						idlist 
						(List.map (fun x -> eval_expr envi x) (ident_exprs exps)))
				  @env) exa
		|_ -> failwith "probleme avec l'application")



let print v = 
	match v with
	 Nat v -> (string_of_int v)
	| _ -> " <function> "

let eval_stat env st =
	match st with
	| ASTEcho (x) -> print_endline (print( eval_expr env x))


let eval_dec (env:env) de =
	match de with
	| ASTConst(x, _, ex) -> (x, (eval_expr env ex))::env
	| ASTFun(x, _, arg, ex) -> (x, (Ferm (ex, (ident_arg arg), env)))::env
	| ASTFRec(x, _, arg, ex) -> (x, (FermR (ex, x, (ident_arg arg), env)))::env


let rec eval_cmds env cm =
	match cm with
	| ASTStat(x)-> eval_stat env x
	| ASTStatCmds(x,cms) -> eval_stat env x ; eval_cmds env cms
	| ASTDecCmds(x,cms) -> eval_dec env x ; eval_cmds env cms


let eval_prog prog =
	match prog with
	| ASTProg(x)-> eval_cmds [] x


let _ =
try
let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
let e = Parser.prog Lexer.token lexbuf in
eval_prog e;
print_char '\n'
with Lexer.Eof -> exit 0