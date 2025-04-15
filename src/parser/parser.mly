%{
  open Ast
%}

%token DECL_TYPES DECL_CONSTS DECL_LAWS DECL_INIT
%token LPAREN RPAREN COLON
%token EOF

%token PERSISTENT EXCLUSIVE
%token TYPE_PROP TYPE_IPROP
%token FALSE STAR WAND BOX TOPLEFTCORNER TOPRIGHTCORNER

%token <string> IDENT

(** Low precedence *)
%right WAND
%left STAR
%nonassoc BOX
(** High precedence *)

%start <instance> instance

%%

instance:
| option(decl_types) decl_consts decl_laws decl_init EOF
  {
    let decl_types = 
      match $1 with
      | Some decl_types -> decl_types
      | None -> []
    in
    { decl_types;
      decl_consts = $2;
      decl_laws = $3;
      decl_init = $4; }
  }

decl_types:
| DECL_TYPES list(IDENT)
  { $2 }

decl_consts:
| DECL_CONSTS list(decl_const)
  { $2 }

decl_const:
| IDENT COLON itype
  { $1, $3 }

decl_laws:
| DECL_LAWS list(decl_law)
  { $2 }

decl_init:
| DECL_INIT list(iprop)
  { $2 }

itype:
| TYPE_PROP
  { Tprop }
| TYPE_IPROP
  { Tiprop }
| IDENT
  { Tcustom $1 }

decl_law:
| iprop
  { Box $1 }
| prop
  { Pure $1 }
| TOPLEFTCORNER prop TOPRIGHTCORNER
  { Pure $2 }
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

prop:
| PERSISTENT IDENT
  { let atom = Atom $2 in
    Persistent atom }
