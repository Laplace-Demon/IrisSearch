type t = Finite of int | Infinite

exception Underflow

let one = Finite 1
let inf = Infinite
let is_finite = function Finite _ -> true | Infinite -> false
let is_infinite t = not (is_finite t)

let to_int = function
  | Finite i -> i
  | Infinite -> raise (Invalid_argument "Multiplicity.to_int")

let to_string = function Finite i -> Int.to_string i | Infinite -> "inf"

let add t1 t2 =
  match (t1, t2) with
  | Infinite, _ | _, Infinite -> Infinite
  | Finite i1, Finite i2 -> Finite (i1 + i2)

let sub t1 t2 =
  match (t1, t2) with
  | Infinite, _ -> Some Infinite
  | Finite i1, Finite i2 ->
      let c = Int.compare i1 i2 in
      if c = 0 then None
      else if c > 0 then Some (Finite (i1 - i2))
      else raise Underflow
  | _, _ -> raise Underflow

let mul t1 t2 =
  match (t1, t2) with
  | Infinite, _ | _, Infinite -> Infinite
  | Finite i1, Finite i2 -> Finite (i1 * i2)

let div t1 t2 =
  match (t1, t2) with
  | Infinite, _ -> Infinite
  | _, Infinite -> raise Underflow
  | Finite i1, Finite i2 ->
      let c = Int.div i1 i2 in
      if c >= 1 then Finite c else raise Underflow

let equal t1 t2 =
  match (t1, t2) with
  | Infinite, Infinite -> true
  | Finite i1, Finite i2 -> Int.equal i1 i2
  | _, _ -> false

let compare t1 t2 =
  match (t1, t2) with
  | Infinite, Infinite -> 0
  | Infinite, _ -> 1
  | _, Infinite -> -1
  | Finite i1, Finite i2 -> Int.compare i1 i2

let min t1 t2 =
  match (t1, t2) with
  | Infinite, _ -> t2
  | _, Infinite -> t1
  | Finite i1, Finite i2 -> Finite (Int.min i1 i2)

let hash = function Infinite -> 42 | Finite i -> i
