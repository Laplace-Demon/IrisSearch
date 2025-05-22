open Format

type stat = {
  mutable generated_state_count : int;
  generated_state_size_distribution : (int * int, int) Hashtbl.t;
  mutable visited_state_count : int;
  mutable maximum_search_depth : int;
  search_depth_distribution : (int, int) Hashtbl.t;
  mutable duplication_count : int;
  operation_distribution : (string, int) Hashtbl.t;
}

let stat_recorder =
  {
    generated_state_count = 0;
    generated_state_size_distribution = Hashtbl.create 17;
    visited_state_count = 0;
    maximum_search_depth = 0;
    search_depth_distribution = Hashtbl.create 17;
    duplication_count = 0;
    operation_distribution = Hashtbl.create 17;
  }

let reset () =
  stat_recorder.generated_state_count <- 0;
  Hashtbl.reset stat_recorder.generated_state_size_distribution;
  stat_recorder.visited_state_count <- 0;
  stat_recorder.maximum_search_depth <- 0;
  Hashtbl.reset stat_recorder.search_depth_distribution;
  stat_recorder.duplication_count <- 0;
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
  stat_recorder.maximum_search_depth <-
    Int.max stat_recorder.maximum_search_depth depth;
  match Hashtbl.find_opt stat_recorder.search_depth_distribution depth with
  | Some count ->
      Hashtbl.replace stat_recorder.search_depth_distribution depth (count + 1)
  | None -> Hashtbl.add stat_recorder.search_depth_distribution depth 1

let record_duplication () =
  stat_recorder.duplication_count <- stat_recorder.duplication_count + 1

let record_operation oper =
  match Hashtbl.find_opt stat_recorder.operation_distribution oper with
  | Some count ->
      Hashtbl.replace stat_recorder.operation_distribution oper (count + 1)
  | None -> Hashtbl.add stat_recorder.operation_distribution oper 1

let pp_stat ?(avg = 1) fmt () =
  let {
    generated_state_count;
    generated_state_size_distribution;
    visited_state_count;
    maximum_search_depth;
    search_depth_distribution;
    duplication_count;
    operation_distribution;
  } =
    stat_recorder
  in
  fprintf fmt
    "@[<v 4>statistics@,\
     generated state count: %i@,\
     visited state count: %i@,\
     maximum search depth: %i@,\
     duplication count: %i@,\
     @]@[<v 8>operations count:@,\
     %a@]@.\n"
    (Int.div generated_state_count avg)
    (Int.div visited_state_count avg)
    (Int.div maximum_search_depth avg)
    (Int.div duplication_count avg)
    (pp_print_seq (fun fmt (oper, count) ->
         fprintf fmt "%s: %i" oper (Int.div count avg)))
    (Hashtbl.to_seq operation_distribution)
