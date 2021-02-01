open Common

let f (g, a, b, c) = g a b c

let rec mine block pair difficulty =
  let _hash = block_fingerprint block in
  if hash_is_solution _hash difficulty then
    _hash
  else
    mine {block with nonce = block.nonce + 2} pair difficulty

let mine_blocklist blocklist pair difficulty =
  let rec loop blocklist =
    match blocklist with
      b::rest -> 
        if not pair then
          b.nonce <- 1;
        let _hash = mine b pair difficulty in
        print_endline "POW!";
        loop rest
      | [] -> ()
  in
  loop blocklist

let () =
  print_endline "Toy program to test mining in two parallel threads";

  let n = 5 in
  let blocklist = make_block_list n in
  let _ = Thread.create f (mine_blocklist, blocklist, true, 3) in
  (*let _ = Thread.create f (mine_blocklist, blocklist, false, 1) in*)
  ignore(read_line())
