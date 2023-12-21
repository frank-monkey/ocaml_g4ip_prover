type prop = 
  | Atom of string
  | And of prop * prop
  | Or of prop * prop
  | Implies of prop * prop
  | True
  | False

exception UncaughtCase

let rec prove (p : prop) : bool = rInv ([], [], p)

and rInv (o, d, p) = 
  match p with
  | And(p1, p2) -> rInv (o, d, p1) && rInv (o, d, p2)
  | True -> true
  | Implies(p1, p2) -> rInv (p1::o, d, p2)
  | _ -> lInv (o, d, p)

and lInv (o, d, p) = 
  match o with 
  | Atom(a) :: o' -> lInv (o', Atom(a)::d, p)
  | And(a, b) :: o' -> lInv(a::b::o', d, p)
  | Or(a, b) :: o' -> lInv (o', d, a) || rInv (o', d, b)
  | Implies(True, a) :: o' -> lInv (a::o', d, p)
  | Implies(False, _) :: o' -> lInv (o', d, p)
  | Implies(And(a, b), c) :: o' -> lInv (Implies(a, Implies(b, c))::o', d, p)
  | Implies(Atom(a), b) :: o' -> lInv (o', Implies(Atom(a), b)::d, p)
  | Implies(Implies(a, b), c) :: o' -> lInv (o', Implies(Implies(a, b), c)::d, p)
  | True :: o' -> lInv (o', d, p)
  | False :: _ -> true
  | [] -> searchR(d, p) || searchL([], d, p)
  | _ -> raise UncaughtCase

and searchR (d, p) =
  match p with
  | Atom(a) -> List.mem (Atom(a)) d
  | Or(a, b) -> rInv ([], d, a) || rInv ([], d, b)
  | _ -> false

and searchL (used_d, unused_d, p) =
  match unused_d with 
  | [] -> false
  | new_prop :: unused_d' -> 
    let s = searchL' (new_prop, (used_d)@(unused_d'), p)
    in if s then true else searchL (new_prop::used_d, unused_d', p)

and searchL' (new_prop, d, p) =
  match new_prop with
  | Implies(Implies(a, b), c) -> rInv ([Implies(b,c); a], d, b) && lInv ([c], d, p)
  | Implies(Atom(p), a) -> List.mem(Atom(p)) d && lInv ([a], d, Atom(p))
  | _ -> false
  
  

