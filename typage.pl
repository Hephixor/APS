oprim(X) :- member(X,[not,and,or,eq,lt,add,sub,mul,div]).
tprim(X) :- member(X,[bool,int]).
num(X) :- integer(X).
ident(X) :- string_codes(S,X), string(S).
sym(X) :- tprim(X);oprim(X);ident(X).
addEnv(G,[],G).
addEnv(G,[X|T],[X|GG]) :- addEnv(G,T,GG).
checkEnv([(X,T)|_],X,T).
checkEnv([(_,_)|G],X,T) :- checkEnv(G,X,T).
checkArgs([],[]).
checkArgs([(_,T)|Q],[T|S]) :- checkArgs(Q,S).
checkTypes(_,[],[]).
checkTypes(G,[E|R],[T|RR]) :- expr(G,E,T), checkTypes(G,R,RR).
typeExpr(_,true,bool).
typeExpr(_,false,bool).
typeExpr(_,N,int) :- integer(N).
typeExpr([(E,TT)|_],E,T) :- TT=T.
typeExpr([(ES,_)|C],E,T) :- not(ES=E), typeExpr(C,E,T).
typeExpr(C,not(A),bool) :- typeExpr(C,A,bool).
typeExpr(C,and(A,B),bool) :- typeExpr(C,A,bool), typeExpr(C,B,bool).
typeExpr(C,or(A,B),bool) :- typeExpr(C,A,bool), typeExpr(C,B,bool).
typeExpr(C,eq(A,B),bool) :- typeExpr(C,A,bool), typeExpr(C,B,bool).
typeExpr(C,eq(A,B),bool) :- typeExpr(C,A,int), typeExpr(C,B,int).
typeExpr(C,mul(A,B),int) :- typeExpr(C,A,int), typeExpr(C,B,int).
typeExpr(C,sub(A,B),int) :- typeExpr(C,A,int), typeExpr(C,B,int).
typeExpr(C,div(A,B),int) :- typeExpr(C,A,int), typeExpr(C,B,int).
typeExpr(C,add(A,B),int) :- typeExpr(C,A,int), typeExpr(C,B,int).
typeExpr(C,lt(A,B),bool) :- typeExpr(C,A,int), typeExpr(C,B,int).
typeExpr(C,if(A, B, D),T) :- typeExpr(C,A,bool),typeExpr(C,B,T),typeExpr(C,D,T).
typeExpr(C,abstr(ARGS,E),fleche(TS,T)) :- checkArgs(ARGS,TS), addEnv(C,ARGS,CC), typeExpr(CC,E,T).
typeExpr(C,app(E,ES),T) :- checkArgs(ES,TS),typeExpr(C,E,fleche(TS,T)).

typeStat(C,echo(E),void) :- typeExpr(C,E,int).
typeStat(C,set(I,E),void) :- checkEnv(C,I,T), typeExpr(C,E,T).
typeStat(C,call(I,ES),void) :- checkArgs(ES,TS),typeExpr(C,I,fleche(TS,void)).
typeStat(C,ifst(E,prog(CM),prog(CS)),void) :- typeExpr(C,E,bool), typeCmds(C,CM,void), typeCmds(C,CS,void).
typeStat(C,while(E,prog(CS)),void) :- typeExpr(C,E,bool), typeCmds(C,CS,void).


typeDec(C,const(X,T,E),CC) :- typeExpr(C,E,T), addEnv(C,[(X,T)],CC).
typeDec(C,funrec(X, ARGS, T, E), CCC) :- checkArgs(ARGS,TS), addEnv(C,ARGS,CC), typeExpr(CC,E,T), addEnv(CC,[(X,fleche(TS,T))],CCC).
typeDec(C,fun(X, ARGS, T, E), CCC) :- checkArgs(ARGS,TS), addEnv(C,ARGS,CC), typeExpr(CC,E,T) , addEnv(C,[(X,fleche(TS,T))],CCC).
typeDec(C,var(X,T),CC) :- addEnv(C,[(X,T)],CC).
typeDec(C,proc(X,ARGS,prog(P)),CCC) :- checkArgs(ARGS,TS), addEnv(C,ARGS,CC), typeCmds(CC,P,void) , addEnv(C,[(X,fleche(TS,void))],CCC).
typeDec(C,procrec(X,ARGS,prog(P)),CCC):- checkArgs(ARGS,TS), addEnv(C,ARGS,CC), typeCmds(CC,P,void), addEnv(CC,[(X,fleche(TS,T))],CCC).

typeCmds(_,[],void).
typeCmds(C,[S|CS],void) :- typeStat(C,S,void), typeCmds(C,CS,void).
typeCmds(C,[D|CS],void) :- typeDec(C,D,CC), typeCmds(CC,CS,void).

typeProg(CS, void) :- typeCmds([], CS, void).
