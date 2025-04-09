open Format

type stat = {
  mutable state_count : int;
  mutable state_size_distribution : (int, int) Hashtbl.t;
  mutable maximum_search_depth : int;
  mutable duplication_count : int;
}

let stat_recorder =
  {
    state_count = 0;
    state_size_distribution = Hashtbl.create 17;
    maximum_search_depth = 0;
    duplication_count = 0;
}

let record_state size =
  stat_recorder.state_count <- stat_recorder.state_count + 1;
  match Hashtbl.find_opt stat_recorder.state_size_distribution size with
  | Some count -> Hashtbl.replace stat_recorder.state_size_distribution size (count + 1)
  | None -> Hashtbl.add stat_recorder.state_size_distribution size 1

let record_depth depth =
  stat_recorder.maximum_search_depth <- max depth stat_recorder.maximum_search_depth

let record_duplication () =
  stat_recorder.duplication_count <- stat_recorder.duplication_count + 1

let pp_stat fmt =
  let { state_count ; state_size_distribution ; maximum_search_depth ; duplication_count } = stat_recorder in
  fprintf fmt "statistics@.";
  fprintf fmt "state count: %i@." state_count;
  fprintf fmt "state size distribution: %a@." (pp_print_seq (fun fmt (size, count) -> fprintf fmt "(%i : %i)" size count)) (Hashtbl.to_seq state_size_distribution);
  fprintf fmt "maximum search depth: %i@." maximum_search_depth;
  fprintf fmt "duplication count: %i@." duplication_count;
