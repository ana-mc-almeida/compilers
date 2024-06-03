%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;          /* integer value */
  double                d;          /* double value */
  std::string          *s;          /* symbol name or string literal */
  cdk::basic_node      *node;       /* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;
  til::block_node                               *block;
  std::vector<std::shared_ptr<cdk::basic_type>> *type_vec;
};

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING tPOINTER

%token tTYPE_INT tTYPE_DOUBLE tTYPE_STRING tTYPE_VOID // types
%token tEXTERNAL tFORWARD tPUBLIC tPRIVATE tVAR // declarations
%token tLOOP tSTOP tNEXT tRETURN // instructions loop
%token tBLOCK tIF tPRINT tPRINTLN
%token tREAD tFUNCTION tSET tSIZEOF tNULL tINDEX tOBJECTS// expressions
%token tPROGRAM // program
%token tGE tLE tEQ tNE tAND tOR // logical expressions 
%token tUNLESS tITERATE tFOR tEXECUTE
// TODO add missing tokens

%nonassoc tIFX

%nonassoc tUNARY
%nonassoc '('

%type <expression> expr function_definition
%type <lvalue> lval

%type <block> block inner_block
%type <sequence> fdeclarations declarations instructions expressions function_args
%type <node> fdeclaration declaration program instruction conditional_instruction function_arg
%type <type> type void_type data_type function_type function_return_type
%type <type_vec> types

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : fdeclarations program  { compiler->ast(new cdk::sequence_node(LINE, $2, $1)); }
     | fdeclarations          { compiler->ast($1); }
     | program                { compiler->ast(new cdk::sequence_node(LINE, $1)); }
     | /* empty */            { compiler->ast(new cdk::sequence_node(LINE)); }
     ;

fdeclarations : fdeclarations fdeclaration   { $$ = new cdk::sequence_node(LINE, $2, $1); }
              | fdeclaration                 { $$ = new cdk::sequence_node(LINE, $1); }
              ;

fdeclaration : tEXTERNAL type tIDENTIFIER ')'     { $$ = new til::declaration_node(LINE, $2, tEXTERNAL, *$3, nullptr); delete $3; }
             | tFORWARD type tIDENTIFIER ')'      { $$ = new til::declaration_node(LINE, $2, tFORWARD,  *$3, nullptr); delete $3; }
             | tPUBLIC type tIDENTIFIER ')'       { $$ = new til::declaration_node(LINE, $2, tPUBLIC,  *$3, nullptr); delete $3; }
             | /* private */ declaration          { $$ = $1; }
             | tPUBLIC type tIDENTIFIER expr ')'  { $$ = new til::declaration_node(LINE, $2, tPUBLIC, *$3, $4); delete $3; }
             | tPUBLIC tVAR tIDENTIFIER expr ')'  { $$ = new til::declaration_node(LINE, nullptr, tPUBLIC, *$3, $4); delete $3; }
             | tPUBLIC tIDENTIFIER expr ')'       { $$ = new til::declaration_node(LINE, nullptr, tPUBLIC, *$2, $3); delete $2; }
             ;

program : tPROGRAM inner_block ')'   { $$ = new til::function_definition_node(LINE, $2); }
        ;

function_definition : tFUNCTION '(' function_return_type ')' inner_block ')'               { $$ = new til::function_definition_node(LINE, new cdk::sequence_node(LINE), $3, $5); }
                    | tFUNCTION '(' function_return_type function_args ')' inner_block ')'  { $$ = new til::function_definition_node(LINE,$4, $3, $6); }
                    ;

block : tBLOCK inner_block ')' { $$ = $2; }
      ;

inner_block : declarations instructions { $$ = new til::block_node(LINE, $1, $2); }
            | declarations              { $$ = new til::block_node(LINE, $1, new cdk::sequence_node(LINE)); }
            | instructions              { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), $1); }
            | /* empty */               { $$ = new til::block_node(LINE, new cdk::sequence_node(LINE), new cdk::sequence_node(LINE)); }
            ;

declarations : declarations declaration { $$ = new cdk::sequence_node(LINE, $2, $1); }
             | declaration              { $$ = new cdk::sequence_node(LINE, $1); }
             ;

declaration : '(' type tIDENTIFIER ')'       { $$ = new til::declaration_node(LINE, $2, tPRIVATE, *$3, nullptr); delete $3; }
            | '(' type tIDENTIFIER expr ')'  { $$ = new til::declaration_node(LINE, $2, tPRIVATE, *$3, $4); delete $3; }
            | tVAR tIDENTIFIER expr ')'      { $$ = new til::declaration_node(LINE, nullptr, tPRIVATE, *$2, $3); delete $2; }
            ;

function_type : '(' function_return_type ')'               { $$ = cdk::functional_type::create($2); }
              | '(' function_return_type '(' types ')' ')'     { $$ = cdk::functional_type::create(*$4, $2); delete $4; }

function_return_type : type        { $$ = $1; }
                     | tTYPE_VOID  { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
                     ;

type : data_type { $$ = $1; }
     | void_type { $$ = $1; }
     ;

void_type : void_type tPOINTER     { $$ = $1; }
          | tTYPE_VOID tPOINTER    { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)); }
          ;

data_type : tTYPE_INT         { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
          | tTYPE_DOUBLE      { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
          | tTYPE_STRING      { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
          | function_type     { $$ = $1;}
          | data_type tPOINTER     { $$ = cdk::reference_type::create(4, $1); }
          ;

types : types type  { $$ = $1; $$->push_back($2); }
      | type        { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(1, $1); }
      ;

instructions : instructions instruction { $$ = new cdk::sequence_node(LINE, $2, $1); }
             | instruction              { $$ = new cdk::sequence_node(LINE, $1); }
             ;

// FIXME not sure is im not missing an ')' on 'instruction : expr'
instruction : expr                           { $$ = new til::evaluation_node(LINE, $1); } 
            | tPRINT expressions ')'         { $$ = new til::print_node(LINE, $2, false); }
            | tPRINTLN expressions ')'       { $$ = new til::print_node(LINE, $2, true); }
            | tSTOP tINTEGER ')'             { $$ = new til::stop_node(LINE, $2); }
            | tSTOP ')'                      { $$ = new til::stop_node(LINE, 1); }
            | tNEXT tINTEGER ')'             { $$ = new til::next_node(LINE, $2); }
            | tNEXT ')'                      { $$ = new til::next_node(LINE, 1); }
            | tRETURN expr ')'               { $$ = new til::return_node(LINE, $2); }
            | tRETURN ')'                    { $$ = new til::return_node(LINE, nullptr); }
            | block                          { $$ = $1; } 
            | conditional_instruction        { $$ = $1; } 
            | tLOOP expr instruction ')'     { $$ = new til::loop_node(LINE, $2, $3); }
            | '(' tUNLESS expr tITERATE expr tFOR expr tEXECUTE expr')' { $$ = new til::unless_iterate_node(LINE,$3,  $5, $7, $9); }
            ;

conditional_instruction : tIF expr instruction ')' %prec tIFX    { $$ = new til::if_node(LINE, $2, $3); }
                        | tIF expr instruction instruction ')'   { $$ = new til::if_else_node(LINE, $2, $3, $4); } 
                        ;

expressions : expressions expr    { $$ = new cdk::sequence_node(LINE, $2, $1); }
            | expr                { $$ = new cdk::sequence_node(LINE, $1); }
            ;

expr : tINTEGER                              { $$ = new cdk::integer_node(LINE, $1); }
     | tDOUBLE                               { $$ = new cdk::double_node(LINE, $1); }
     | tSTRING                               { $$ = new cdk::string_node(LINE, $1); }
     | '(' '-' expr ')'                      { $$ = new cdk::unary_minus_node(LINE, $3); }
     | '(' '+' expr ')'                      { $$ = new cdk::unary_plus_node(LINE, $3); }
     | '(' '?' lval ')'                      { $$ = new til::address_node(LINE, $3); }
     | '(' '+' expr expr ')'                 { $$ = new cdk::add_node(LINE, $3, $4); }
     | '(' '-' expr expr ')'                 { $$ = new cdk::sub_node(LINE, $3, $4); }
     | '(' '*' expr expr ')'                 { $$ = new cdk::mul_node(LINE, $3, $4); }
     | '(' '/' expr expr ')'                 { $$ = new cdk::div_node(LINE, $3, $4); }
     | '(' '%' expr expr ')'                 { $$ = new cdk::mod_node(LINE, $3, $4); }
     | '(' '<' expr expr ')'                 { $$ = new cdk::lt_node(LINE, $3, $4); }
     | '(' '>' expr expr ')'                 { $$ = new cdk::gt_node(LINE, $3, $4); }
     | '(' tGE expr expr ')'                 { $$ = new cdk::ge_node(LINE, $3, $4); }
     | '(' tLE expr expr ')'                 { $$ = new cdk::le_node(LINE, $3, $4); }
     | '(' tNE expr expr ')'                 { $$ = new cdk::ne_node(LINE, $3, $4); }
     | '(' tEQ expr expr ')'                 { $$ = new cdk::eq_node(LINE, $3, $4); }
     | '(' '~' expr ')'                      { $$ = new cdk::not_node(LINE, $3); }
     | '(' tAND expr expr ')'                { $$ = new cdk::and_node(LINE, $3, $4); }
     | '(' tOR expr expr ')'                 { $$ = new cdk::or_node(LINE, $3, $4); } 
     | lval                                  { $$ = new cdk::rvalue_node(LINE, $1); }
     | tSET lval expr ')'                    { $$ = new cdk::assignment_node(LINE, $2, $3); }
     | tSIZEOF expr ')'                      { $$ = new til::sizeof_node(LINE, $2); }
     | tNULL                                 { $$ = new til::null_node(LINE); }
     | tREAD                                 { $$ = new til::read_node(LINE); }
     | tOBJECTS expr ')'                     { $$ = new til::alloc_node(LINE, $2); }
     | '(' expr expressions ')'              { $$ = new til::function_call_node(LINE, $2, $3); }  
     | '(' expr ')'                          { $$ = new til::function_call_node(LINE, $2, new cdk::sequence_node(LINE)); }  
     | '(' '@' expressions ')'      { $$ = new til::function_call_node(LINE, nullptr, $3); }
     | '(' '@'  '(' ')' ')'                  { $$ = new til::function_call_node(LINE, nullptr, new cdk::sequence_node(LINE)); }
     | function_definition                   { $$ = $1; }
     ;

lval : tIDENTIFIER            { $$ = new cdk::variable_node(LINE, $1); }
     | tINDEX expr expr ')'   { $$ = new til::index_node(LINE, $2, $3); }
     ;

function_args : function_args '(' function_arg ')'     { $$ = new cdk::sequence_node(LINE, $3, $1); }
              | '(' function_arg ')'                   { $$ = new cdk::sequence_node(LINE, $2); }
              ;

function_arg : type tIDENTIFIER    { $$ = new til::declaration_node(LINE, $1, tPRIVATE, *$2, nullptr); delete $2; }
             ;


%%
