%{
  open Ast
%}

%token <string> ATOM
%token STAR WAND FALSE
%token LPAREN RPAREN
%token EOF

%left STAR
%right WAND

%start <instance> instance

%%

instance:
| list(iprop) EOF       { $1 }

iprop:
| LPAREN iprop RPAREN   { $2 }
| ATOM                  { Atom $1 }
| iprop STAR iprop      { Star ($1, $3) }
| iprop WAND iprop      { Wand ($1, $3) }
| FALSE                 { False }
