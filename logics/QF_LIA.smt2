(logic QF_LIA

 :smt-lib-version 2.0
 :written_by "Cesare Tinelli"
 :date "2010-04-30"

 :theories (Ints)

 :language 
 "Closed quantifier-free formulas built over an arbitrary expansion of the
  Ints signature with free constant symbols, but whose terms of sort Int 
  are all linear, that is, have no occurrences of the function symbols
  *, /, div, mod, and abs, except as specified the :extensions attribute.
 "

 :extensions
 "Terms with _concrete_ coefficients are also allowed, that is, terms
  of the form c, (* c x), or (* x c)  where x is a free constant and 
  c is a term of the form n or (- n) for some numeral n.
 "
)


