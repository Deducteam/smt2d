%{
  open Syntax
%}

%token EOF

%token OPEN CLOSE
%token <string> NUMERAL DECIMAL HEXADECIMAL BINARY STRING SYMBOL KEYWORD

%token UNDERSCORE

%token AS

%token LET FORALL EXISTS BANG

%token TRUE FALSE

%token PRINT_SUCCESS EXPAND_DEFINITIONS INTERACTIVE_MODE PRODUCE_PROOFS 
PRODUCE_UNSAT_CORES PRODUCE_MODELS PRODUCE_ASSIGNMENTS REGULAR_OUTPUT_CHANNEL 
DIAGNOSTIC_OUTPUT_CHANNEL RANDOM_SEED VERBOSITY

%token ERROR_BEHAVIOR NAME AUTHORS VERSION STATUS REASON_UNKNOWN ALL_STATISTICS

%token SET_LOGIC SET_OPTION SET_INFO DECLARE_SORT DEFINE_SORT DECLARE_FUN 
DEFINE_FUN PUSH POP ASSERT CHECK_SAT GET_ASSERTIONS GET_PROOF GET_UNSAT_CORE 
GET_VALUE GET_ASSIGNMENT GET_OPTION GET_INFO EXIT

%start command term
%type <Syntax.command> command
%type <Syntax.term> term
  
%%

numeral_plus :
  | NUMERAL                 
  | NUMERAL numeral_star    
;

symbol_star :
  |                       
  | SYMBOL symbol_star    
;

spec_constant :
  | NUMERAL        
  | DECIMAL        
  | HEXADECIMAL    
  | BINARY         
  | STRING         
;

s_expr
  | spec_constant             
  | symbol                    
  | keyword                   
  | OPEN s_expr_star CLOSE    
;

s_expr_star
  |                       
  | s_expr s_expr_star    
;

identifier :
  | SYMBOL                            
  | OPEN SYMBOL numeral_plus CLOSE    
;

sort :
  | identifier                         
  | OPEN identifier sort_plus CLOSE    
;

sort_star :
  |                   
  | sort sort_plus    
;

sort_plus :
  | sort              
  | sort sort_plus    
;

attribute_value :
  | spec_constant             
  | SYMBOL                    
  | OPEN s_expr_star CLOSE    
;

attribute :
  | KEYWORD                    
  | KEYWORD attribute_value    
;

attribute_plus :
  | attribute                   
  | attribute attribute_plus    
;

qual_identifier :
  | identifier                       
  | OPEN AS identifier sort CLOSE    
;

var_binding :
  | OPEN SYMBOL term CLOSE    
;

var_binding_plus
  | var_binding                     
  | var_binding var_binding_plus    
;

sorted_var :
  | OPEN SYMBOL sort CLOSE    
;

sorted_var_star :
  |                    
  | sorted_var_star    
;

sorted_var_plus :
  | sorted_var                    
  | sorted_var sorted_var_plus    
;

term :
  | spec_constant                                        
  | qual_identifier                                      
  | OPEN qual_identifier term_plus CLOSE                 
  | OPEN LET OPEN var_binding_plus CLOSE term CLOSE      
  | OPEN FORALL OPEN sorted_var_plus CLOSE term CLOSE    
  | OPEN EXISTS OPEN sorted_var_plus CLOSE term CLOSE    
  | OPEN BANG term attribute_plus CLOSE                  
;

term_plus :
  | term              
  | term term_plus    
;

b_value :
  | TRUE     
  | FALSE    
;

option :
  | PRINT_SUCCESS b_value               
  | EXPAND_DEFINITIONS b_value          
  | INTERACTIVE_MODE b_value            
  | PRODUCE_PROOFS b_value              
  | PRODUCE_UNSAT_CORES b_value         
  | PRODUCE_MODELS b_value              
  | PRODUCE_ASSIGNMENTS b_value         
  | REGULAR_OUTPUT_CHANNEL STRING       
  | DIAGNOSTIC_OUTPUT_CHANNEL STRING    
  | RANDOM_SEED NUMERAL                 
  | VERBOSITY NUMERAL                   
  | attribute                           
;

info_flag :
  | ERROR_BEHAVIOR     {  }
  | NAME               {  }
  | AUTHORS            {  }
  | VERSION            {  }
  | STATUS             {  }
  | REASON_UNKNOWN     {  }
  | KEYWORD            {  }
  | ALL_STATISTICS     {  }                                                        
;

command :
  | OPEN SET_LOGIC SYMBOL CLOSE                                        {  }
  | OPEN SET_OPTION option CLOSE                                       {  }
  | OPEN SET_INFO attribute CLOSE                                      {  }
  | OPEN DECLARE_SORT SYMBOL NUMERAL CLOSE                             {  }
  | OPEN DEFINE_SORT SYMBOL OPEN symbol_star CLOSE sort CLOSE          {  }
  | OPEN DECLARE_FUN SYMBOL OPEN sort_star CLOSE sort CLOSE            {  }
  | OPEN DEFINE_FUN SYMBOL OPEN sorted_var_star CLOSE sort term CLOSE  {  }
  | OPEN PUSH NUMERAL CLOSE                                            {  }
  | OPEN POP NUMERAL CLOSE                                             {  }
  | OPEN ASSERT term CLOSE                                             {  }
  | OPEN CHECK_SAT CLOSE                                               {  }
  | OPEN GET_ASSERTIONS CLOSE                                          {  }
  | OPEN GET_PROOF CLOSE                                               {  }
  | OPEN GET_UNSAT_CORE CLOSE                                          {  }
  | OPEN GET_VALUE OPEN term_plus CLOSE CLOSE                          {  }
  | OPEN GET_ASSIGNMENT CLOSE                                          {  }
  | OPEN GET_OPTION KEYWORD CLOSE                                      {  }
  | OPEN GET_INFO info_flag CLOSE                                      {  }
  | OPEN EXIT CLOSE                                                    {  }
;

%%
