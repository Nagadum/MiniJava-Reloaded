%{
  open AST
  open Location

  let parse_error str =
    Error.syntax (symbol_loc())

  let mkexp d = 
    { 
      edesc = d;
      eloc  = symbol_loc();
      etype = None;
    }

  let mkval v = 
    { 
      edesc = Val v;
      eloc  = symbol_loc();
      etype = None;
    }

  let mkexploc d loc = 
    { 
      edesc = d;
      eloc  = loc;
      etype = None;
    }

  let mkclass n p a m = 
    { 
      cname = n;
      cparent = p;
      cattributes = a;
      cmethods = m;
      cloc  = symbol_loc();
    }

  let mkatt n t s d = 
    {
      aname = n;
      atype = t;
      astatic = s;
      adefault = d;
      aloc  = symbol_loc();
    }

  let mkmeth n r a s b =
    {
      mname = n;
      mreturntype = r;
      margstype = a;
      mstatic = s;
      mbody = b;
      mloc  = symbol_loc();
    }
%}

/* Les tokens */

%token EOF

%token <int> INT
%token <string> LIDENT
%token <string> UIDENT
%token <string> STRING

%token LPAREN		
%token RPAREN		
%token LBRACE		
%token RBRACE		
%token SEMI
%token DOT
%token COMMA
%token EQUAL

%token CLASS
%token ELSE
%token EXTENDS
%token FALSE
%token IF
%token IN
%token INSTANCEOF
%token NEW
%token NULL
%token STATIC
%token THIS
%token TRUE

%token AND
%token OR
%token NOT
%token GT
%token GE
%token LT
%token LE
%token EQ
%token NEQ
%token PLUS MINUS 
%token TIMES DIV MOD


%right SEMI
%right EQUAL 
%left OR
%left AND
%left EQ NEQ                /*r ["=="] and ["!="] */
%left GT GE LT LE INSTANCEOF
%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS NOT CAST
%nonassoc PAREXP
%left DOT

%start start
%type <AST.astprogram> start
%%

start:
  | CLASS UIDENT parent LBRACE classBody RBRACE start
      { 
	let cl,e = $7 in
	let al,ml = $5 in
	  (mkclass $2 $3 al ml):: cl, e
      }
  | expression EOF
      { [], Some $1 }
  | EOF 
      { [], None }
;

parent:
  | EXTENDS UIDENT
      { $2 }
  | /* nothing */
      { "Object" }
;

classBody:
  | static UIDENT LIDENT initialization SEMI classBody
      {

	let al,ml = $6 in 
	  (mkatt $3 $2 $1 $4) :: al, ml
      }
  | static UIDENT LIDENT LPAREN argumentso RPAREN LBRACE expression RBRACE
      classBody
      {
	let al,ml = $10 in 
	  al, (mkmeth $3 $2 $5 $1 $8) :: ml
      }
  | /* empty */
      { [],[] }
;

static:
  | STATIC        { true }
  | /* nothing */ { false }

initialization:
  | EQUAL expression { Some $2 }
  | /* nothing */    { None }
;

argumentso:
  | arguments
      { $1 }
  | /* nothing */
      { [] }
;

arguments:
  | arguments COMMA UIDENT LIDENT
      { ($4,$3)::$1 }
  | UIDENT LIDENT
      { [$2,$1] }
;

expression:
  | NOT expression
      { mkexp (Call($2,"not",[])) }
  | expression MINUS expression
      { mkexp (Call($1,"sub",[$3])) }
  | expression PLUS expression
      { mkexp (Call($1,"add",[$3])) }
  | expression TIMES expression
      { mkexp (Call($1,"mul",[$3])) }
  | expression DIV expression
      { mkexp (Call($1,"div",[$3])) }
  | expression MOD expression
      { mkexp (Call($1,"mod",[$3])) }
  | expression AND expression
      { mkexp (Call($1,"and",[$3])) }
  | expression OR expression
      { mkexp (Call($1,"or",[$3])) }
  | expression GT expression
      { mkexp (Call($1,"gt",[$3])) }
  | expression GE expression
      { mkexp (Call($1,"ge",[$3])) }
  | expression LT expression
      { mkexp (Call($1,"lt",[$3])) }
  | expression LE expression
      { mkexp (Call($1,"le",[$3])) }
  | expression EQ expression
      { mkexp (Call($1,"eq",[$3])) }
  | expression NEQ expression
      { mkexp (Call($1,"neq",[$3])) }
  | expression SEMI expression
      { mkexp (Seq($1,$3)) }
  | MINUS expression %prec UMINUS
      { mkexp (Call($2,"neg",[])) }
  | IF LPAREN expression RPAREN LBRACE expression RBRACE elseo
      { match $8 with
	  | None -> mkexp (If($3, $6, None))
	  | Some e -> mkexp (If($3, $6, Some e))
      }
  | LIDENT EQUAL expression
      { mkexp (Assign($1,$3)) }
  | UIDENT LIDENT EQUAL expression IN LBRACE expression RBRACE
      { mkexp (Define($2,$1,$4,$7)) }
  | NEW UIDENT 
      { mkexp (New $2) }
  | expression DOT LIDENT LPAREN paramso RPAREN        
      { mkexp (Call($1,$3,$5)) }
  | LPAREN UIDENT RPAREN expression %prec CAST
      { mkexp (Cast($2,$4)) }
  | expression INSTANCEOF UIDENT
      { mkexp (Instanceof($1,$3)) }
  | LIDENT
      { mkexp (Var $1) }
  | THIS
      { mkexp (Var "this") }
  | value
      { $1 }
  | LPAREN expression RPAREN %prec PAREXP
      { $2 }
;

elseo:
  | ELSE LBRACE expression RBRACE
      { Some $3 }
  | /* nothing */
      { None }
;

paramso:
  | params
      { $1 }
  | /* nothing */
      { [] }
;

params:
  | params COMMA expression
      { $3::$1 }
  | expression
      { [$1] }
;

value:
  | INT    { mkval (Int $1) }
  | STRING { mkval (String $1) }
  | NULL   { mkval (Null) }
  | TRUE   { mkval (Boolean true) }
  | FALSE  { mkval (Boolean false) }
;
