open Format

type itype =
  | Tprop
  | Tiprop
  | Tcustom of string
  | Tarrow of itype list * itype
  | Tlaw

let rec itype_eqb ity1 ity2 =
  match (ity1, ity2) with
  | Tprop, Tprop -> true
  | Tiprop, Tiprop -> true
  | Tcustom str1, Tcustom str2 -> String.equal str1 str2
  | Tarrow (ity_list1, ity1), Tarrow (ity_list2, ity2) ->
      List.equal itype_eqb ity_list1 ity_list2 && itype_eqb ity1 ity2
  | Tlaw, Tlaw -> true
  | _, _ -> false

let rec pp_itype fmt = function
  | Tprop -> fprintf fmt "Prop"
  | Tiprop -> fprintf fmt "iProp"
  | Tcustom str -> fprintf fmt "%s" str
  | Tarrow (ity_list, ity) ->
      fprintf fmt "%a -> %a"
        (fun fmt l ->
          match l with
          | [] -> pp_print_string fmt "..."
          | _ ->
              pp_print_list
                ~pp_sep:(fun fmt () -> fprintf fmt " * ")
                pp_itype fmt l)
        ity_list pp_itype ity
  | Tlaw -> fprintf fmt "law"

let group_typed_str : (string * itype) list -> (string list * itype) list =
  let rec group_typed_str_aux acc typed_str_list =
    match typed_str_list with
    | [] -> (
        match acc with
        | None -> []
        | Some (acc_list, ity) -> [ (List.rev acc_list, ity) ])
    | (str, ity') :: typed_str_list' -> (
        match acc with
        | None -> group_typed_str_aux (Some ([ str ], ity')) typed_str_list'
        | Some (acc_list, ity) ->
            if itype_eqb ity ity' then
              group_typed_str_aux (Some (str :: acc_list, ity)) typed_str_list'
            else
              (List.rev acc_list, ity)
              :: group_typed_str_aux (Some ([ str ], ity')) typed_str_list')
  in
  group_typed_str_aux None

let pp_typed_strs ?(pp_paren = false) fmt (strs_list, ity) =
  if pp_paren then
    fprintf fmt "(%a : %a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)
      strs_list pp_itype ity
  else
    fprintf fmt "%a : %a"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)
      strs_list pp_itype ity

let pp_typed_strs_list ?(pp_sep = pp_print_cut) ?(pp_paren = false) () fmt
    typed_strs_list =
  let pp_paren = pp_paren && List.length typed_strs_list > 1 in
  fprintf fmt "%a"
    (pp_print_list ~pp_sep (pp_typed_strs ~pp_paren))
    typed_strs_list
