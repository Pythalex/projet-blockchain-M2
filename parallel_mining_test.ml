open Common

let f (g, a, b, c, d, e, f) = g a b c d e f

let sync_set boolref mutex =
  Mutex.lock mutex;
  boolref := not !boolref;
  Mutex.unlock mutex

let rec mine block pair difficulty other_has_found mutex =
  let found = !other_has_found in
  if found then None
  else
    let hash = block_fingerprint block in
    if hash_is_solution hash difficulty then Some (block.nonce, hash)
    else
      mine
        { block with nonce = block.nonce + 2 }
        pair difficulty other_has_found mutex

let mine_blocklist_thread blocklist pair difficulty found_pair found_impair
    mutex =
  let my_name = if pair then " pair " else "impair" in
  let other_name = if pair then "impair" else " pair " in
  let i_found = if pair then found_pair else found_impair in
  let other_has_found = if pair then found_impair else found_pair in
  let first = ref false in

  let rec loop blocklist =
    match blocklist with
    | b :: rest ->
        let block = if not pair then { b with nonce = 1 } else b in

        (match mine block pair difficulty other_has_found mutex with
        | Some (nonce, hash) ->
            Mutex.lock mutex;
            if !other_has_found then (
              first := false;
              other_has_found := false)
            else (
              i_found := true;
              first := true);
            Mutex.unlock mutex;
            block.nonce <- nonce
        | None ->
            first := false;
            sync_set other_has_found mutex);

        Mutex.lock mutex;
        if !first then (
          Printf.printf "[%s] Block %d | Gagné | POW with nonce = %d\n%!"
            my_name block.id block.nonce;
          flush stdout)
        else (
          Printf.printf
            "[%s] Block %d | Perdu | Le thread %s a trouvé avant moi, Skip.\n\
             %!"
            my_name block.id other_name;
          flush stdout);
        Mutex.unlock mutex;

        loop rest
    | [] -> ()
  in

  loop blocklist

let mine_blocklist blocklist difficulty =
  let pair_has_found = ref false in
  let impair_has_found = ref false in
  let m = Mutex.create () in
  let thread_pair =
    Thread.create f
      ( mine_blocklist_thread,
        blocklist,
        true,
        difficulty,
        pair_has_found,
        impair_has_found,
        m )
  in
  let thread_impair =
    Thread.create f
      ( mine_blocklist_thread,
        blocklist,
        false,
        difficulty,
        pair_has_found,
        impair_has_found,
        m )
  in
  Thread.join thread_pair;
  Thread.join thread_impair

let () =
  print_endline "Toy program to test mining in two parallel threads";

  let n = 5 in
  let blocklist = make_block_list n in
  mine_blocklist blocklist 1;
  (*let _ = Thread.create f (mine_blocklist, blocklist, false, 1) in*)
  ignore (read_line ())
