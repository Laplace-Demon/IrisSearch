module OptionMonad = struct
  type 'a t = 'a Option.t

  let return = Option.some
  let bind = Option.bind
  let ( let* ) = bind
end

module ListMonad = struct
  type 'a t = 'a list

  let return x = [ x ]
  let bind l f = List.concat_map f l
  let ( let* ) = bind
end
