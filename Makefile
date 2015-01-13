.PHONY: all clean bench cleanbench

SMTLIBDIR = smtlib2/QF_UF
BENCHDIR = bench
BENCHSMTS = $(shell find $(BENCHDIR) -name "*.smt2")
BENCHDKS_NEEDED = $(BENCHSMTS:.smt2=.dk)
BENCHDKS = $(shell find $(BENCHDIR) -name "*.dk")
BENCHDKTS_NEEDED = $(BENCHDKS:.dk=.dkt)

all: check logic.dko

%.dko: %.dk
	dkcheck -e $<

check: *.ml *.mli *.mll *.mly
	ocamlbuild check.native
	mv check.native check

clean:
	rm -f check logic.dko *~ *\#
	ocamlbuild -clean

%.dkt: %.dk
	dkcheck $<

%.dk: %.smt2
	./check $< > $@

bench: all $(BENCHDIR)/.dummy $(BENCHDKS_NEEDED) $(BENCHDKTS_NEEDED)

$(BENCHDIR)/.dummy:
	[ -e $(BENCHIR) ] || mkdir $(BENCHDIR)
	cp -r $(SMTLIBDIR) $(BENCHDIR)
	touch $(BENCHDIR)/.dummy

cleanbench:
	rm -fr bench
