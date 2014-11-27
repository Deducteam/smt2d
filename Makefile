.PHONY: all clean

all: smt2check

smt2check: *.ml *.mli *.mll *.mly
	ocamlbuild smt2check.native
	mv smt2check.native smt2check

clean:
	rm -f smt2check *~ *\#
	ocamlbuild -clean