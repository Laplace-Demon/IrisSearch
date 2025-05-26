%{
  open Ast
  open Format
  open Type
%}

%token DECL_TYPES DECL_FUNCS DECL_PREDS DECL_CONSTS DECL_FACTS DECL_LAWS DECL_INIT
%token LPAREN RPAREN COLON COMMA VERTICAL_BAR
%token EOF

%token PERSISTENT EXCLUSIVE
%token TYPE_PROP TYPE_IPROP
%token FALSE STAR WAND BOX TOPLEFTCORNER TOPRIGHTCORNER
%token NOT AND OR ARROW
%token FORALL EXISTS
%token EQ NEQ

%token <string> IDENT

(** Low precedence *)
%nonassoc FORALL EXISTS
%right WAND
%left STAR
%nonassoc BOX

%right ARROW
%left OR
%left AND
%nonassoc NOT PERSISTENT EXCLUSIVE
(** High precedence *)

%start <instance> instance

%%

instance:
| option(decl_types) option(decl_funcs) option(decl_preds) option(decl_consts) option(decl_facts) option(decl_laws) decl_init EOF
  {
    let decl_types = 
      match $1 with
      | Some decl_types -> decl_types
      | None -> []
    in
    let decl_funcs = 
      match $2 with
      | Some decl_funcs -> decl_funcs
      | None -> []
    in
    let decl_preds =
      match $3 with
      | Some decl_preds -> decl_preds
      | None -> []
    in
    let decl_consts =
      match $4 with
      | Some decl_consts -> decl_consts
      | None -> []
    in
    let decl_facts =
      match $5 with
      | Some decl_facts -> decl_facts
      | None -> []
    in
    let decl_laws =
      match $6 with
      | Some decl_laws -> decl_laws
      | None -> []
    in
    { decl_types;
      decl_funcs;
      decl_preds;
      decl_consts;
      decl_facts;
      decl_laws;
      decl_init = $7; }
  }

decl_types:
| DECL_TYPES list(decl_type)
  { $2 }

decl_type:
| predefined_itype
  { let open Validate in
    raise (DuplicateDeclarationError (asprintf "%a" pp_symbol_kind Type, asprintf "%a" pp_itype $1)) }
| IDENT
  { ($1, []) }
| IDENT EQ nonempty_list(decl_constr)
  { ($1, $3) }

decl_constr:
| VERTICAL_BAR IDENT COLON itype
  { ($2, $4) }

decl_funcs:
| DECL_FUNCS list(decl_func)
  { $2 }

decl_func:
| IDENT COLON itype
  { ($1, $3) }

decl_preds:
| DECL_PREDS list(decl_pred)
  { $2 }

decl_pred:
| IDENT COLON itype
  { ($1, $3) }

decl_consts:
| DECL_CONSTS list(decl_const)
  { List.concat_map (fun (str_list, ity) -> List.map (fun str -> (str, ity)) str_list) $2 }

decl_const:
| nonempty_list(IDENT) COLON itype
  { $1, $3 }

decl_facts:
| DECL_FACTS separated_list(COMMA, decl_fact)
  { $2 }

decl_fact:
| prop
  { $1 }

decl_laws:
| DECL_LAWS separated_list(COMMA, decl_law)
  { $2 }

decl_law:
| iprop
  { None, $1 }
| IDENT COLON iprop
  { Some $1, $3 }

decl_init:
| DECL_INIT separated_list(COMMA, iprop)
  { $2 }

term:
| IDENT
  { Var $1 }
| IDENT LPAREN separated_nonempty_list(COMMA, term) RPAREN
  { App ($1, $3) }

prop:
| LPAREN prop RPAREN
  { $2 }
| PERSISTENT iprop %prec PERSISTENT
  { Persistent $2 }
| NOT prop
  { Not $2 }
| prop AND prop
  { And ($1, $3) }
| prop OR prop
  { Or ($1, $3) }
| prop ARROW prop
  { Imply ($1, $3) }
| IDENT nonempty_list(term)
  { Pred ($1, $2) }
| FORALL IDENT COLON itype COMMA prop %prec FORALL
  { Forall ([$2, $4], $6) }
| FORALL nonempty_list(binders) COMMA prop %prec FORALL
  { let typed_str_list = List.concat_map (fun (str_list, ity) -> List.map (fun str -> str, ity) str_list) $2 in
    Forall (typed_str_list, $4) }
| EXISTS IDENT COLON itype COMMA prop %prec EXISTS
  { Exists ([$2, $4], $6) }
| EXISTS nonempty_list(binders) COMMA prop %prec EXISTS
  { let typed_str_list = List.concat_map (fun (str_list, ity) -> List.map (fun str -> str, ity) str_list) $2 in
    Exists (typed_str_list, $4) }
| term EQ term
  { Eq ($1, $3) }
| term NEQ term
  { Neq ($1, $3) }

iprop:
| LPAREN iprop RPAREN
  { $2 }
| FALSE
  { False }
| IDENT
  { Atom $1 }
| TOPLEFTCORNER prop TOPRIGHTCORNER
  { Pure $2 }
| iprop STAR iprop
  { Star ($1, $3) }
| iprop OR iprop
  { HOr ($1, $3) }
| iprop WAND iprop
  { Wand ($1, $3) }
| EXCLUSIVE iprop %prec EXCLUSIVE
  { Wand (Star ($2, $2), False) }
| BOX iprop
  { Box $2 }
| IDENT nonempty_list(term) 
  { HPred ($1, $2) }
| FORALL IDENT COLON itype COMMA iprop %prec FORALL
  { HForall ([$2, $4], $6) }
| FORALL nonempty_list(binders) COMMA iprop %prec FORALL
  { let typed_str_list = List.concat_map (fun (str_list, ity) -> List.map (fun str -> str, ity) str_list) $2 in
    HForall (typed_str_list, $4) }
| EXISTS IDENT COLON itype COMMA iprop %prec EXISTS
  { HExists ([$2, $4], $6) }
| EXISTS nonempty_list(binders) COMMA iprop %prec EXISTS
  { let typed_str_list = List.concat_map (fun (str_list, ity) -> List.map (fun str -> str, ity) str_list) $2 in
    HExists (typed_str_list, $4) }

binders:
| LPAREN nonempty_list(IDENT) COLON itype RPAREN
  { $2, $4 }

itype:
| simple_itype
  { $1 }
| separated_nonempty_list(STAR, simple_itype) ARROW simple_itype
  { Tarrow ($1, $3) }

simple_itype:
| predefined_itype
  { $1 }
| IDENT
  { Tcustom $1 }

predefined_itype:
| TYPE_PROP
  { Tprop }
| TYPE_IPROP
  { Tiprop }
