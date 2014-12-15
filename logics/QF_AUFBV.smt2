(logic QF_AUFBV

:smt-lib-version 2.0
:written_by "Cesare Tinelli and Clark Barrett"
:date "2010-05-11"
:last_updated "2013-06-24"
:history 
 "2013-06-24:
  Changed references to Fixed_Size_Bitvectors to FixedSizeBitVectors.
"

:theories (FixedSizeBitVectors ArraysEx)

:language 
 "Closed quantifier-free formulas built over an arbitrary expansion of the
  FixedSizeBitVectors and ArraysEx signatures with free sort and function
  symbols, but with the restriction that all array terms have sort of the 
  form (Array (_ BitVec i) (_ BitVec j)) for some i, j > 0.
 "

:extensions "As in the logic QF_BV."
)

