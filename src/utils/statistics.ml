open Format

type stat = {
  mutable generated_state_count : int;
  generated_state_size_distribution : (int * int, int) Hashtbl.t;
  mutable visited_state_count : int;
  search_depth_distribution : (int, int) Hashtbl.t;
  mutable subsumption_count : int;
  operation_distribution : (string, int) Hashtbl.t;
}

let stat_recorder =
  {
    generated_state_count = 1;
    generated_state_size_distribution = Hashtbl.create 17;
    visited_state_count = 0;
    search_depth_distribution = Hashtbl.create 17;
    subsumption_count = 0;
    operation_distribution = Hashtbl.create 17;
  }

let reset () =
  stat_recorder.generated_state_count <- 1;
  Hashtbl.reset stat_recorder.generated_state_size_distribution;
  stat_recorder.visited_state_count <- 0;
  Hashtbl.reset stat_recorder.search_depth_distribution;
  stat_recorder.subsumption_count <- 0;
  Hashtbl.reset stat_recorder.operation_distribution

let record_generated_state size =
  stat_recorder.generated_state_count <- stat_recorder.generated_state_count + 1;
  match
    Hashtbl.find_opt stat_recorder.generated_state_size_distribution size
  with
  | Some count ->
      Hashtbl.replace stat_recorder.generated_state_size_distribution size
        (count + 1)
  | None -> Hashtbl.add stat_recorder.generated_state_size_distribution size 1

let record_visited_state () =
  stat_recorder.visited_state_count <- stat_recorder.visited_state_count + 1

let record_depth depth =
  match Hashtbl.find_opt stat_recorder.search_depth_distribution depth with
  | Some count ->
      Hashtbl.replace stat_recorder.search_depth_distribution depth (count + 1)
  | None -> Hashtbl.add stat_recorder.search_depth_distribution depth 1

let record_subsumption () =
  stat_recorder.subsumption_count <- stat_recorder.subsumption_count + 1

let record_operation oper =
  match Hashtbl.find_opt stat_recorder.operation_distribution oper with
  | Some count ->
      Hashtbl.replace stat_recorder.operation_distribution oper (count + 1)
  | None -> Hashtbl.add stat_recorder.operation_distribution oper 1

let pp_stat ?(avg = 1) fmt () =
  let {
    generated_state_count;
    visited_state_count;
    subsumption_count;
    operation_distribution;
    _;
  } =
    stat_recorder
  in
  fprintf fmt
    "@[<v 4>statistics@,\
     generated state count: %i@,\
     visited state count: %i@,\
     subsumption count: %i@,\
     @]@[<v 8>operations count:@,\
     %a@]@\n"
    (Int.div generated_state_count avg)
    (Int.div visited_state_count avg)
    (Int.div subsumption_count avg)
    (pp_print_seq (fun fmt (oper, count) ->
         fprintf fmt "%s: %i" oper (Int.div count avg)))
    (Hashtbl.to_seq operation_distribution)
