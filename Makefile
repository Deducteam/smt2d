.PHONY: all clean bench

SMTLIBDIR = smtlib2/QF_UF
BENCHDIR = bench
BENCHSMTS = $(shell find $(BENCHDIR) -name "*.smt2")
BENCHCHECKS_NEEDED = $(BENCHSMTS:.smt2=.chk)

all: smt2check

smt2check: *.ml *.mli *.mll *.mly
	ocamlbuild smt2check.native
	mv smt2check.native smt2check

clean:
	rm -f smt2check *~ *\#
	ocamlbuild -clean

%.chk: %.smt2
	./smt2check $<

bench: smt2check $(BENCHDIR)/.dummy $(BENCHCHECKS_NEEDED)

$(BENCHDIR)/.dummy:
	[ -e $(BENCHIR) ] || mkdir $(BENCHDIR)
	cp -r $(SMTLIBDIR) $(BENCHDIR)
	touch $(BENCHDIR)/.dummy

cleanbench:
	rm -fr bench
