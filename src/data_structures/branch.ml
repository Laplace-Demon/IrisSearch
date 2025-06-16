open Path

module type Type = sig
  type t
end

module Make (State : Type) = struct
  type state = State.t

  type t = {
    mutable solved : bool;
    pred : t option;
    mutable succ : t list;
    mutable path : state list * string;
  }

  let create () = { solved = false; pred = None; succ = []; path = ([], "") }

  let add br cnt =
    let succ = List.init cnt (fun _ -> { (create ()) with pred = Some br }) in
    br.succ <- succ;
    succ

  let is_marked { solved; _ } = solved

  let rec fwd_propagate br =
    if not br.solved then (
      br.solved <- true;
      List.iter fwd_propagate br.succ)

  let rec bwd_propagate br =
    if List.for_all is_marked br.succ then (
      br.solved <- true;
      fwd_propagate br;
      match br.pred with None -> () | Some pred -> bwd_propagate pred)

  let mark br path msg =
    br.path <- (List.rev path, msg);
    fwd_propagate br;
    match br.pred with None -> () | Some pred -> bwd_propagate pred

  let rec get_path br =
    let path, msg = br.path in
    Path (path, msg, List.map get_path br.succ)
end
