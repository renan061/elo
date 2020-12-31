#
# Makefile
#

RM= rm -f
OCAMLC= ocamlfind ocamlopt -linkpkg -package llvm

main:
	$(OCAMLC) elo.cmx test.ml -o test.out
	./test.out

clean:
	$(RM) *.o *.out *.cmi *.cmo *.cmx
