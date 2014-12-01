.PHONY: all clean bench cleanbench

SMTLIBDIR = smtlib2/QF_UF
BENCHDIR = bench
BENCHSMTS = $(shell find $(BENCHDIR) -name "*.smt2")
BENCHCHECKS_NEEDED = $(BENCHSMTS:.smt2=.chk)

all: check

check: *.ml *.mli *.mll *.mly
	ocamlbuild check.native
	mv check.native check

clean:
	rm -f check *~ *\#
	ocamlbuild -clean

%.chk: %.smt2
	./check $<

bench: check $(BENCHDIR)/.dummy $(BENCHCHECKS_NEEDED)

$(BENCHDIR)/.dummy:
	[ -e $(BENCHIR) ] || mkdir $(BENCHDIR)
	cp -r $(SMTLIBDIR) $(BENCHDIR)
	touch $(BENCHDIR)/.dummy

cleanbench:
	rm -fr bench
