open Is
open Ast
open Freshname

module Negative = struct
  let const_num = ref 20
  let law_num = ref 50
  let law_size = ref 10
  let pure_num = ref 5
  let init_num = ref 10
  let set_const_num n = const_num := n
  let set_law_num n = law_num := n
  let set_law_size n = law_size := n
  let set_pure_num n = pure_num := n
  let set_init_num n = init_num := n

  let generate () =
    let () = reset () in
    let () = Random.self_init () in
    let consts_array =
      Array.init !const_num (fun _ -> generate ~base:"Atom" ())
    in
    let decl_types = [] in
    let decl_consts =
      List.map (fun str -> (str, Tiprop)) (Array.to_list consts_array)
    in
    let persistent_laws =
      repeat
        (fun () ->
          let left_size = 1 + Random.int (!law_size - 1) in
          let right_size = !law_size - left_size in
          let left_list =
            repeat
              (fun () -> Atom consts_array.(Random.int !const_num))
              left_size
          in
          let right_list =
            repeat
              (fun () -> Atom consts_array.(Random.int !const_num))
              right_size
          in
          let left =
            List.fold_left
              (fun acc ipr -> Star (acc, ipr))
              (List.hd left_list) (List.tl left_list)
          in
          let right =
            List.fold_left
              (fun acc ipr -> Star (acc, ipr))
              (List.hd right_list) (List.tl right_list)
          in
          Box (Wand (left, right)))
        !law_num
    in
    let pure_laws =
      repeat
        (fun () ->
          Pure (Persistent (Atom consts_array.(Random.int !const_num))))
        !pure_num
    in
    let decl_laws = persistent_laws @ pure_laws in
    let decl_init =
      repeat (fun () -> Atom consts_array.(Random.int !const_num)) !init_num
    in
    { decl_types; decl_consts; decl_laws; decl_init }
end
