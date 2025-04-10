%{
  open Ast
%}

%token DECL_CONSTS DECL_LAWS DECL_INIT
%token LPAREN RPAREN COLON
%token EOF

%token <string> IDENT
%token PERSISTENT EXCLUSIVE
%token TYPE_PROP TYPE_IPROP
%token STAR WAND BOX FALSE

(** Low precedence *)
%right WAND
%left STAR
%nonassoc BOX
(** High precedence *)

%start <instance> instance

%%

instance:
| decl_consts decl_laws decl_init EOF
  {
    { decl_consts = $1;
      decl_laws = $2;
      decl_init = $3; }
  }

decl_consts:
| DECL_CONSTS list(decl_type)
  { $2 }

decl_laws:
| DECL_LAWS list(decl_law)
  { $2 }

decl_init:
| DECL_INIT list(iprop)
  { $2 }

decl_type:
| IDENT COLON itype
  { $1, $3 }

itype:
| TYPE_PROP
  { Tprop }
| TYPE_IPROP
  { Tiprop }

decl_law:
| iprop
  { Box $1 }
| PERSISTENT IDENT
  { let atom = Atom $2 in
    Pure (Persistent atom) }
| EXCLUSIVE IDENT
  { let atom = Atom $2 in
    Box (Wand (Star (atom, atom), False)) }

iprop:
| LPAREN iprop RPAREN
  { $2 }
| FALSE
  { False }
| IDENT
  { Atom $1 }
| iprop STAR iprop
  { Star ($1, $3) }
| iprop WAND iprop
  { uncurry_wand ($1, $3) }
| BOX iprop
  { Box $2 }
