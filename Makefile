.PHONY: all clean bench cleanbench

SMTLIBDIR = smtlib2/QF_UF/QG-classification/qg5
BENCHDIR = bench
BENCHSMTS = $(shell find $(BENCHDIR) -name "*.smt2")
BENCHDKS_NEEDED = $(BENCHSMTS:.smt2=.dk)
BENCHDKS = $(shell find $(BENCHDIR) -name "*.dk")
BENCHDKTS_NEEDED = $(BENCHDKS:.dk=.dkt)

all: check smt2.dko

%.dko: %.dk
	dkcheck -e $<

check: *.ml *.mli *.mll *.mly
	ocamlbuild check.native
	mv check.native check

clean:
	rm -f check smt2.dko *~ *\#
	ocamlbuild -clean

%.dkt: %.dk
	dkcheck $<

%.dk: %.smt2
	./check $< > $@

bench: check $(BENCHDIR)/.dummy $(BENCHDKS_NEEDED) $(BENCHDKTS_NEEDED)

$(BENCHDIR)/.dummy:
	[ -e $(BENCHIR) ] || mkdir $(BENCHDIR)
	cp -r $(SMTLIBDIR) $(BENCHDIR)
	touch $(BENCHDIR)/.dummy

cleanbench:
	rm -fr bench
