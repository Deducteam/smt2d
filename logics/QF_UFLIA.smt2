(logic QF_UFLIA

 :smt-lib-version 2.0
 :written_by "Cesare Tinelli"
 :date "2011-06-11"

 :theories ( Ints )

 :language 
 "Closed quantifier-free formulas built over arbitrary expansions of the
  Ints signatures with free sort and function symbols, but with the 
  following restrictions:
  - all terms of sort Int are linear, that is, have no occurrences of the
    function symbols *, /, div, mod, and abs, except as specified in the 
    :extensions attributes;
 "

 :extensions
 "As in the logic QF_LIA."
)

