#
# Makefile
#

RM= rm -f

BIN= ../bin/
TOBIN= $(BIN)elo

OCAMLC= ocamlopt
OCAMLC_LLVM= ocamlfind $(OCAMLC) -linkpkg -package llvm
LEX= ocamllex
YACC= menhir --unused-token INVALID

main: ast1 scanner parser ast2 sem backend
	ocamlfind ocamlopt -o $(TOBIN) -linkpkg -package llvm,llvm.bitwriter \
		parser.cmx scanner.cmx sem.cmx backend.cmx elo.ml

scanner: parser
	ocamllex scanner.mll
	ocamlopt parser.cmx scanner.ml

parser: ast1
	menhir --unused-token INVALID --explain parser.mly
	ocamlopt ast1.cmx parser.mli parser.ml

ast1:
	ocamlopt ast1.ml

ast2:
	ocamlfind ocamlopt -linkpkg -package llvm ast2.ml

sem: ast1 ast2
	ocamlfind ocamlopt -linkpkg -package llvm sem.ml

backend: ast2
	ocamlfind ocamlopt -linkpkg -package llvm ast2.cmx backend.ml

pack: clean
	$(OCAMLC) ast1.ml -for-pack Elo
	$(YACC) parser.mly
	$(OCAMLC) parser.mli parser.ml -for-pack Elo
	$(LEX) scanner.mll
	$(OCAMLC) scanner.ml -for-pack Elo
	$(OCAMLC_LLVM) ast2.ml -for-pack Elo
	$(OCAMLC_LLVM) sem.ml -for-pack Elo
	$(OCAMLC_LLVM) backend.ml -for-pack Elo
	$(OCAMLC) -pack -o elo.cmx \
		scanner.cmx parser.cmx ast1.cmx ast2.cmx sem.cmx backend.cmx

clean:
	$(RM) *.o *.out *.cmi *.cmo *.cmx
	$(RM) parser.conflicts parser.ml parser.mli scanner.ml
