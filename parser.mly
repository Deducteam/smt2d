%{
  open Concrete
  open Script
%}

%token EOF

%token OPEN CLOSE
%token <string> NUMERAL DECIMAL HEXADECIMAL BINARY STRING SYMBOL KEYWORD

%token UNDERSCORE

%token AS

%token LET FORALL EXISTS ATTRIBUTE

%token SET_LOGIC SET_OPTION SET_INFO DECLARE_SORT DEFINE_SORT DECLARE_FUN 
DEFINE_FUN PUSH POP ASSERT CHECK_SAT GET_ASSERTIONS GET_PROOF GET_UNSAT_CORE 
GET_VALUE GET_ASSIGNMENT GET_OPTION GET_INFO EXIT

%start command term
%type <Script.command> command
%type <Concrete.term> term
  
%%

numeral_plus:
  | NUMERAL                 { [$1] }
  | NUMERAL numeral_plus    { $1 :: $2 }
;

symbol_star:
  |                       { [] }
  | SYMBOL symbol_star    { $1 :: $2 }
;

spec_constant:
  | NUMERAL        { Numeral $1 }
  | DECIMAL        { Decimal $1 }
  | HEXADECIMAL    { Hexadecimal $1 }
  | BINARY         { Binary $1 }
  | STRING         { String $1 }
;

s_expr:
  | spec_constant             { Spec_constant_expr $1 }
  | SYMBOL                    { Symbol_expr $1 }
  | KEYWORD                   { Keyword_expr $1 }
  | OPEN s_expr_star CLOSE    { List_expr $2 }
;

s_expr_star:
  |                       { [] }
  | s_expr s_expr_star    { $1 :: $2 }
;

identifier:
  | SYMBOL                                       { ($1,[]) }
  | OPEN UNDERSCORE SYMBOL numeral_plus CLOSE    { ($3,$4) }
;

sort:
  | identifier                         { Sort ($1,[]) }
  | OPEN identifier sort_plus CLOSE    { Sort ($2,$3) }
;

sort_star:
  |                   { [] }
  | sort sort_star    { $1 :: $2 }
;

sort_plus:
  | sort              { [$1] }
  | sort sort_plus    { $1 :: $2 }
;

attribute_value:
  | spec_constant             { Spec_constant_value $1 }
  | SYMBOL                    { Symbol_value $1 }
  | OPEN s_expr_star CLOSE    { S_expr_list_value $2 }
;

attribute:
  | KEYWORD                    { ($1,None) }
  | KEYWORD attribute_value    { ($1,Some $2) }
;

attribute_plus:
  | attribute                   { [$1] }
  | attribute attribute_plus    { $1 :: $2 }
;

qual_identifier:
  | identifier                       { ($1,None) }
  | OPEN AS identifier sort CLOSE    { ($3,Some $4) }
;

var_binding:
  | OPEN SYMBOL term CLOSE    { ($2,$3) }
;

var_binding_plus:
  | var_binding                     { [$1] }
  | var_binding var_binding_plus    { $1 :: $2 }
;

sorted_var:
  | OPEN SYMBOL sort CLOSE    { ($2,$3) }
;

sorted_var_star:
  |                               { [] }
  | sorted_var sorted_var_star    { $1 :: $2 }
;

sorted_var_plus:
  | sorted_var                    { [$1] }
  | sorted_var sorted_var_plus    { $1 :: $2 }
;

term:
  | spec_constant                                        { Spec_constant_term $1 }
  | qual_identifier                                      { Qual_identifier_term $1 }
  | OPEN qual_identifier term_plus CLOSE                 { App_term ($2,$3) }
  | OPEN LET OPEN var_binding_plus CLOSE term CLOSE      { Let_term ($4,$6) }
  | OPEN FORALL OPEN sorted_var_plus CLOSE term CLOSE    { Forall_term ($4,$6) }
  | OPEN EXISTS OPEN sorted_var_plus CLOSE term CLOSE    { Exists_term ($4,$6) }
  | OPEN ATTRIBUTE term attribute_plus CLOSE             { Attributed_term ($3,$4) }
;

term_plus:
  | term              { [$1] }
  | term term_plus    { $1 :: $2 }
;

command_option:
  | attribute    { $1 }
;

info_flag:
  | KEYWORD    { $1 }
;

command:
  | OPEN SET_LOGIC SYMBOL CLOSE                                        { Set_logic $3 }
  | OPEN SET_OPTION command_option CLOSE                                       { Set_option $3 }
  | OPEN SET_INFO attribute CLOSE                                      { Set_info $3 }
  | OPEN DECLARE_SORT SYMBOL NUMERAL CLOSE                             { Declare_sort ($3,$4) }
  | OPEN DEFINE_SORT SYMBOL OPEN symbol_star CLOSE sort CLOSE          { Define_sort ($3,$5,$7) }
  | OPEN DECLARE_FUN SYMBOL OPEN sort_star CLOSE sort CLOSE            { Declare_fun ($3,$5,$7) }
  | OPEN DEFINE_FUN SYMBOL OPEN sorted_var_star CLOSE sort term CLOSE  { Define_fun ($3,$5,$7,$8) }
  | OPEN PUSH NUMERAL CLOSE                                            { Push $3 }
  | OPEN POP NUMERAL CLOSE                                             { Pop $3 }
  | OPEN ASSERT term CLOSE                                             { Assert $3 }
  | OPEN CHECK_SAT CLOSE                                               { Check_sat }
  | OPEN GET_ASSERTIONS CLOSE                                          { Get_assertions }
  | OPEN GET_PROOF CLOSE                                               { Get_proof }
  | OPEN GET_UNSAT_CORE CLOSE                                          { Get_unsat_core }
  | OPEN GET_VALUE OPEN term_plus CLOSE CLOSE                          { Get_value $4 }
  | OPEN GET_ASSIGNMENT CLOSE                                          { Get_assignment }
  | OPEN GET_OPTION KEYWORD CLOSE                                      { Get_option $3 }
  | OPEN GET_INFO info_flag CLOSE                                      { Get_info $3 }
  | OPEN EXIT CLOSE                                                    { Exit }
  | EOF                                                                { raise End_of_file }
;

%%
