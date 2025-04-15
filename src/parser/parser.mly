%{
  open Ast
%}

%token DECL_TYPES DECL_PREDS DECL_CONSTS DECL_LAWS DECL_INIT
%token LPAREN RPAREN COLON
%token EOF

%token PERSISTENT EXCLUSIVE
%token TYPE_PROP TYPE_IPROP
%token FALSE STAR WAND BOX TOPLEFTCORNER TOPRIGHTCORNER
%token NOT AND OR ARROW

%token <string> IDENT

(** Low precedence *)
%right WAND
%left STAR
%nonassoc BOX

%right ARROW
%left OR
%left AND
%nonassoc NOT
(** High precedence *)

%start <instance> instance

%%

instance:
| option(decl_types) option(decl_preds) decl_consts decl_laws decl_init EOF
  {
    let decl_types = 
      match $1 with
      | Some decl_types -> decl_types
      | None -> []
    in
    let decl_preds =
      match $2 with
      | Some decl_preds -> decl_preds
      | None -> []
    in
    { decl_types;
      decl_preds;
      decl_consts = $3;
      decl_laws = $4;
      decl_init = $5; }
  }

decl_types:
| DECL_TYPES list(decl_type)
  { $2 }

decl_type:
| IDENT
  { $1 }

decl_preds:
| DECL_PREDS list(decl_pred)
  { $2 }

decl_pred:
| IDENT COLON separated_nonempty_list(STAR, itype) ARROW itype
  { ($1, Tarrow ($3, $5)) }

decl_consts:
| DECL_CONSTS list(decl_const)
  { $2 }

decl_const:
| IDENT COLON itype
  { $1, $3 }

decl_laws:
| DECL_LAWS list(decl_law)
  { $2 }

decl_law:
| iprop
  { Box $1 }
| prop
  { Pure $1 }
| EXCLUSIVE IDENT
  { let atom = Atom $2 in
    Box (Wand (Star (atom, atom), False)) }

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
| TOPLEFTCORNER prop TOPRIGHTCORNER
  { Pure $2 }

prop:
| LPAREN prop RPAREN
  { $2 }
| PERSISTENT IDENT
  { let atom = Atom $2 in
    Persistent atom }
| NOT prop
  { Not $2 }
| prop AND prop
  { And ($1, $3) }
| prop OR prop
  { Or ($1, $3) }
| prop ARROW prop
  { Imply ($1, $3) }

term:
| IDENT
  { Var $1 }
