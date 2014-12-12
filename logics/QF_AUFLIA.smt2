(logic QF_AUFLIA

 :smt-lib-version 2.0
 :written_by "Cesare Tinelli"
 :date "2010-04-30"
 :updated "2011-06-03"
 :update_log 
 "Allowed x to be more than a free constants in terms of the form (* c x) or (*x c),
  in :extensions.
 "

 :theories (Ints ArraysEx)

 :language 
 "Closed quantifier-free formulas built over arbitrary expansions of the
  Ints and ArraysEx signatures with free sort and function symbols, but
  with the following restrictions:
  - all terms of sort Int are linear, that is, have no occurrences of the
    function symbols *, /, div, mod, and abs, except as specified in the 
    :extensions attributes;
  - all array terms have sort (Array Int Int).
 "

 :extensions
 "Terms with _concrete_ integer coefficients are also allowed, that is, terms
  of the form c, (* c x), or (* x c)  where 
  - x is a free constant or a term with top symbol not in Ints, and 
  - c is a term of the form n or (- n) for some numeral n.
 ")

