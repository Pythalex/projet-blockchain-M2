(* Represents the merkle tree components
  - E : empty tree
  - N : Node of type 
    - Inner value Z.t
    - Left child t
    - Right child t
    - The depth of the tree (maximum depth of children + 1)
*)
type t = E | N of Z.t * t * t * int

(* Constant : empty tree *)
let empty = E

(*
  Function: make_leaf
  Make a leaf given a value x of type Z.t
  The leaf has depth 0 (no child).
  The value x is hashed to create the leaf inner value.
*)
let make_leaf x = N(Z.of_int (Z.hash x), E, E ,0)

(*
  Function: make_node
  Create a node given the two tree children.

*)
let make_node t1 t2 =
  match t1, t2 with
  | N(x1, _, _ , lvl1), N(x2,_,_, lvl2) ->
    let lvl = max lvl1 lvl2 + 1 in
    N(Z.of_int (Z.hash (Z.add x1 x2)), t1, t2, lvl)
  | _ -> assert false

(*
  Function: fusion
  Group the trees in the list two by two. Does this only once and
  returns the list.
*)
let rec fusion lt =
  match lt with
  | [] | [_] -> lt
  | t1 :: t2 :: s -> make_node t1 t2 :: fusion s

(*
  Function: merkle_of_list
  Apply fusion l until there is only one tree in the list. Finally, it 
  returns the tree.
*)
let rec merkel_of_list l = 
  match l with
  | [] -> E
  | [t] -> t
  | _ -> merkel_of_list (fusion l)

(*
  Function: os_string
  Returns a big int from a string in base 16 by parsing it

  Arguments:
    s, the base 16 string

  Returns:
    Z.t the big int
*)
let of_string s = Z.of_string_base 16 s

(*
  Function: make
  Get a list of string hashes, convert all hashes to Z big int,
  and returns a merkle tree from them.
*)
let make l =
  let l = List.map of_string l in
  merkel_of_list (List.map make_leaf l)

(*
  Function: hash_root
  Returns the string representation of the inner value of the given Node.
*)
let hash_root t =
  match t with
  | E -> raise Not_found
  | N(x,_,_,_) -> Z.format "%X" x


(*
  Function: proof
  Creates a merkle proof for the node at index i.
  Nodes are indexed by their order of appearance in a left depth-first-search
*)
let rec proof t i =
  match t with
  | E 
  | N(_, E, E, _) -> []
  | N(_, g, d, lvl) ->
    let b = 1 lsl (lvl - 1) in
    if i < b then
      (hash_root d)::(proof g i)
    else
      (hash_root g)::(proof d (i - b))

(*
  Function: authenticate
  Returns True if the given value exist in the merkle tree.

  Arguments:
    tr, The string value that we ask if it belongs to the merkle tree
    pr, The merkle proof as list of hash
    root, The value stored in the root of the merkle tree

  Returns:
    True if the Merkle hash sum of the pr list and tr equals the root 
*)
let authenticate tr pr root =
  let tr = of_string tr in
  let root = of_string root in
  let pr = List.map of_string pr in
  let x =
    List.fold_right
    (fun h x -> (Z.of_int (Z.hash (Z.add h x)))) pr (Z.of_int (Z.hash tr)) in
  x = root

(* Tests *)
let () =
  let merkle_tree = make ["8743b52063cd84097a65d1633f5c74f5"; "01dfae6e5d4d90d9892622325959afbe"; "f0fda58630310a6dd91a7d8f0a4ceda2"] in
  let proof_t = proof merkle_tree 1 in 
  (* let a = Z.of_int 1 in
  Z.print a; *)
  (*false*)
  (* let is_in_tree = authenticate "f0fda58630310a6dd91a7d8f0a4ceda2" proof_t (hash_root merkle_tree) in  *)
  (*true*)
  let is_in_tree = authenticate "8743b52063cd84097a65d1633f5c74f5" proof_t (hash_root merkle_tree) in 
  Printf.printf "Proof of first leaf : ";
  List.iter (Printf.printf "%s ") proof_t;
  Printf.printf " \n%b\n" is_in_tree;
  Printf.printf " \n%b\n" (authenticate "01dfae6e5d4d90d9892622325959afbe" proof_t (hash_root merkle_tree))