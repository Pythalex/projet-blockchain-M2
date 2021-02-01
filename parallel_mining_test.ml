open Common

let f (g, a, b, c, d, e) = g a b c d e

let rec mine block pair difficulty other_has_found =
  if !other_has_found then None
  else
    let hash = block_fingerprint block in
    if hash_is_solution hash difficulty then Some (block.nonce, hash)
    else
      mine
        { block with nonce = block.nonce + 2 }
        pair difficulty other_has_found

let mine_blocklist_thread blocklist pair difficulty found_pair found_impair =
  let my_name = if pair then " pair " else "impair" in
  let other_name = if pair then "impair" else " pair " in
  let i_found = if pair then found_pair else found_impair in
  let other_has_found = if pair then found_impair else found_pair in

  let rec loop blocklist =
    match blocklist with
    | b :: rest ->
        let block = if not pair then { b with nonce = 1 } else b in
        (match mine block pair difficulty other_has_found with
        | Some (nonce, hash) ->
            Printf.printf "[%s] Block %d | Gagné | POW with nonce = %d\n%!" my_name block.id nonce;
            flush stdout;
            i_found := true
        | None ->
            Printf.printf "[%s] Block %d | Perdu | Le thread %s a trouvé avant moi, Skip.\n%!"
              my_name block.id other_name;
            flush stdout;
            other_has_found := false);

        loop rest
    | [] -> ()
  in

  loop blocklist

let mine_blocklist blocklist difficulty =
  let pair_has_found = ref false in
  let impair_has_found = ref false in
  let thread_pair =
    Thread.create f
      (mine_blocklist_thread, blocklist, true, difficulty, pair_has_found, impair_has_found)
  in
  let thread_impair =
    Thread.create f
      (mine_blocklist_thread, blocklist, false, difficulty, pair_has_found, impair_has_found)
  in
  Thread.join thread_pair;
  Thread.join thread_impair

let () =
  print_endline "Toy program to test mining in two parallel threads";

  let n = 5 in
  let blocklist = make_block_list n in
  mine_blocklist blocklist 3;
  (*let _ = Thread.create f (mine_blocklist, blocklist, false, 1) in*)
  ignore (read_line ())
