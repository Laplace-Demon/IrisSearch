%{
  open Ast

  let atom_table = Hashtbl.create 17
%}

%token <string> IDENT
%token STAR WAND BOX FALSE
%token LPAREN RPAREN COLON COMMA PERCENT
%token ATOM_DECL
%token EOF

%left STAR
%right WAND

%start <instance> instance

%%

instance:
| atom_decl PERCENT list(iprop) EOF
  { $3 }

atom_decl:
| ATOM_DECL COLON separated_list(COMMA, IDENT)
  { List.iter (fun id ->
    if Hashtbl.mem atom_table id
    then failwith ("Duplicate atom declaration: " ^ id)
    else Hashtbl.add atom_table id ()
    )$3 }

iprop:
| LPAREN iprop RPAREN
  { $2 }
| IDENT
  { let id = $1 in
    if Hashtbl.mem atom_table id
    then Atom id
    else failwith ("Unknown atom: " ^ id) }
| iprop STAR iprop
  { Star ($1, $3) }
| iprop WAND iprop
  { Wand ($1, $3) }
| BOX iprop
  { Box $2 }
| FALSE
  { False }
