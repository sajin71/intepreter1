%{
    open Expr
%}
%token <int> INT
%token MINUS PLUS TIMES DIV
%token <bool> TRUE 
%token <bool> FALSE
%token <string> ID
%token EQ GT LT
%token AND OR NOT
%token IF THEN ELSE
%token LPAR RPAR
%token LET IN
%token EOL 


%left AND OR NOT
%left EQ GT LT
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS 

%start main 
%type <Expr.expr> main

%% 

main:
expr EOL { $1 }
;

expr:
  | INT                     { EConst(VInt($1)) }
  | TRUE                    { EConst(VBool($1)) }
  | FALSE                   { EConst(VBool($1)) }
  | ID						{ EId(Id($1)) }
  | LPAR expr RPAR          { $2 }
  | expr PLUS expr          { EAdd($1, $3) }
  | expr MINUS expr         { ESub($1, $3) }
  | expr TIMES expr         { EMul($1, $3) }
  | expr DIV expr           { EDiv($1, $3) }
  | expr EQ expr            { EEq($1, $3) }
  | expr GT expr            { EGt($1, $3) }
  | expr LT expr            { ELt($1, $3) }
  | expr AND expr           { EIf($1, $3, EConst(VBool(false))) }
  | expr OR expr            { EIf($1, EConst(VBool(true)), $3) }
  | NOT expr                { EIf($2, EConst(VBool(false)), EConst(VBool(true))) }
  | IF expr THEN expr ELSE expr { EIf($2, $4, $6) }
  | LET expr EQ expr IN expr { ELet($2, $4, $6) }
  | MINUS expr %prec UMINUS { EMul(EConst(VInt(-1)) ,$2) }
;
