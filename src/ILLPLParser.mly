%{

module ILLPL = ILLPLSyntax

%}

%token <string> NUMLIT
%token <string> VAR

%token TRUE
%token FALSE
%token CONJ
%token DISJ
%token IMPLIES
%token ADD
%token SUB
%token MUL
%token DIV
%token EQEQ
%token NEQ
%token LT
%token LE
%token GT
%token GE

%token FUN
%token VAL
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token FORALL
%token EXISTS
%token ASSERT

%token REQUIRES
%token ENSURES

%token EQ
%token COMMA
%token SEMI
%token DOT
%token DOTDOT
%token BANG
%token LPAREN
%token RPAREN
%token LSQUARE
%token RSQUARE
%token PIPE

%token EOF

%start prog
%type < ILLPLSyntax.binding list * ILLPLSyntax.expr > prog

%nonassoc quantifier
%nonassoc IN
%nonassoc ELSE
%nonassoc SEMI
%nonassoc ASSERT
%right IMPLIES
%left DISJ
%left CONJ
%nonassoc EQEQ NEQ LT LE GT GE
%left ADD SUB
%left MUL DIV
%nonassoc unary_neg BANG
%nonassoc LSQUARE

%%

prog :
  | bs=funbindings e=valbinding EOF
      { (bs, e) }

expr :
  | simple_expr { $1 }

  | SUB e=expr
    %prec unary_neg
      { ILLPL.Unop (ILLPL.Neg, e) }
  | BANG e=expr { ILLPL.Unop (ILLPL.Not, e) }

  | e1=expr CONJ    e2=expr { ILLPL.And (e1, e2)              }
  | e1=expr DISJ    e2=expr { ILLPL.Or (e1, e2)               }
  | e1=expr IMPLIES e2=expr { ILLPL.Implies (e1, e2)          }
  | e1=expr ADD     e2=expr { ILLPL.Binop (ILLPL.Add, e1, e2) }
  | e1=expr SUB     e2=expr { ILLPL.Binop (ILLPL.Sub, e1, e2) }
  | e1=expr MUL     e2=expr { ILLPL.Binop (ILLPL.Mul, e1, e2) }
  | e1=expr DIV     e2=expr { ILLPL.Binop (ILLPL.Div, e1, e2) }
  | e1=expr EQEQ    e2=expr { ILLPL.Binop (ILLPL.Eq,  e1, e2) }
  | e1=expr NEQ     e2=expr { ILLPL.Binop (ILLPL.Neq, e1, e2) }
  | e1=expr LT      e2=expr { ILLPL.Binop (ILLPL.Lt,  e1, e2) }
  | e1=expr LE      e2=expr { ILLPL.Binop (ILLPL.Le,  e1, e2) }
  | e1=expr GT      e2=expr { ILLPL.Binop (ILLPL.Gt,  e1, e2) }
  | e1=expr GE      e2=expr { ILLPL.Binop (ILLPL.Ge,  e1, e2) }
  | e1=expr SEMI    e2=expr { ILLPL.Sequence (e1, e2)         }

  | e1=expr LSQUARE e2=expr RSQUARE { ILLPL.Index (e1, e2) }
  | e1=expr LSQUARE e2=expr DOTDOT RSQUARE { ILLPL.Slice (e1, e2) }

  | PIPE e=expr PIPE  { ILLPL.Length e }

  | ASSERT e=expr { ILLPL.Assert e }

  | FORALL v=VAR DOT e=expr
    %prec quantifier
      { ILLPL.Forall (v, e) }
  | EXISTS v=VAR DOT e=expr
    %prec quantifier
      { ILLPL.Exists (v, e) }

  | f=VAR LPAREN args=expr_list RPAREN   { ILLPL.Funcall (f, args) }
  | IF e1=expr THEN e2=expr ELSE e3=expr { ILLPL.If (e1, e2, e3) }
  | LET v=VAR EQ e=expr IN body=expr     { ILLPL.Let (v, e, body) }

simple_expr :
  | TRUE                            { ILLPL.Bool true                         }
  | FALSE                           { ILLPL.Bool false                        }
  | NUMLIT                          { ILLPL.Num (Pervasives.int_of_string $1) }
  | VAR                             { ILLPL.Var  $1                           }
  | LPAREN RPAREN                   { ILLPL.Unit                              }
  | LPAREN e=expr RPAREN          { e                                      }
  | LSQUARE es=expr_list RSQUARE  { ILLPL.List es                           }

expr_list :
  | es=separated_list(COMMA, expr) { es }

requires :
  |                 { ILLPL.Requires (ILLPL.Bool true) }
  | REQUIRES e=expr { ILLPL.Requires e                 }

ensures :
  |                          { ILLPL.Ensures ("_", ILLPL.Bool true) }
  | ENSURES e=expr           { ILLPL.Ensures ("_", e)               }
  | ENSURES r=VAR DOT e=expr { ILLPL.Ensures (r, e)                 }

var_list :
  | vs=separated_list(COMMA, VAR) { vs }

funbinding :
  | FUN f=VAR LPAREN vs=var_list RPAREN req=requires ens=ensures EQ body=expr
    { ILLPL.Fun (f, vs, req, ens, body) }

funbindings :
  | funbinding* { $1 }

valbinding :
  | VAL v=VAR EQ e=expr { if v <> "_" then raise (Parser.ParseError "val binding should bind '_'") else e }

%%