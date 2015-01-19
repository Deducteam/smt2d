.PHONY: all lib install clean bench cleanbench

SMTLIBDIR = smtlib2/QF_UF/PEQ
BENCHDIR = bench
BENCHSMTS = $(shell find $(BENCHDIR) -name "*.smt2")
BENCHDKS_NEEDED = $(BENCHSMTS:.smt2=.dk)
BENCHDKS = $(shell find $(BENCHDIR) -name "*.dk")
BENCHDKTS_NEEDED = $(BENCHDKS:.dk=.dkt)

all: check lib logic.dko

lib: *.ml *.mli *.mll *.mly
	ocamlbuild -cflags -w,+a smt2d.cma smt2d.cmxa

check: *.ml *.mli *.mll *.mly
	ocamlbuild -cflags -w,+a check.native
	mv check.native check

install:
	ocamlfind remove smt2d
	ocamlfind install smt2d META _build/smt2d.cma _build/smt2d.cmxa \
		_build/process_script.cmi _build/translate.cmi

%.dko: %.dk
	dkcheck -e $<

%.dkt: %.dk
	dkcheck $<

%.dk: %.smt2
	./check $< > $@

clean:
	rm -f check logic.dko *~ *\#
	ocamlbuild -clean

bench: all $(BENCHDIR)/.dummy $(BENCHDKS_NEEDED) $(BENCHDKTS_NEEDED)

$(BENCHDIR)/.dummy:
	[ -e $(BENCHIR) ] || mkdir $(BENCHDIR)
	cp -r $(SMTLIBDIR) $(BENCHDIR)
	touch $(BENCHDIR)/.dummy

cleanbench:
	rm -fr bench
