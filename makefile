LEX_ML = ocamllex
YACC_ML = /usr/local/bin/ocamlyacc
OCAMLC = ocamlc
toProlog: parser toProlog.ml #myeval.ml
	$(OCAMLC) -o toProlog ast.cmo lexer.cmo parser.cmo toProlog.ml
	$(OCAMLC) -o myeval ast.cmo lexer.cmo parser.cmo myeval.ml
parser: ast.ml lexer.mll parser.mly 
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml
clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm -f toProlog
	rm -f myeval
tidy:
	rm -f *.cmo
	rm -f *.cmi
	rm -f toProlog
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
