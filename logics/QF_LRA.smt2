(logic QF_LRA

 :smt-lib-version 2.0
 :written_by "Cesare Tinelli"
 :date "2010-04-14"
 :updated "2011-06-03"
 :update_log 
 "2011-06-03 Replaced "(* c x), or (* x c)" with "c, (* c x), or (* x c)" in :extensions.
  (the missing case was had been left out unintentionally)
 "

 :theories (Reals)

 :language 
 "Closed quantifier-free formulas built over arbitrary expansions of 
  the Reals signature with free constant symbols, but containing only
  linear atoms, that is, atoms with no occurrences of the function
  symbols * and /, except as specified the :extensions attribute.
 "

 :extensions
 "Terms with _concrete_ coefficients are also allowed, that is, terms
  of the form c, (* c x), or (* x c)  where x is a free constant and 
  c is an integer or rational coefficient. 
  - An integer coefficient is a term of the form m or (- m) for some
    numeral m.
  - A rational coefficient is a term of the form d, (- d) or (/ c n) 
    for some decimal d, integer coefficient c and numeral n other than 0.
 "
)


