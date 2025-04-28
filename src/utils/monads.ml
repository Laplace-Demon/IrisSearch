module ListMonad = struct
  type 'a t = 'a list

  let return x = [ x ]
  let bind l f = List.concat_map f l
  let ( let* ) = bind
  let map l f = List.map f l
  let ( let+ ) = map
  let fail = []
  let choose = ( @ )
end
