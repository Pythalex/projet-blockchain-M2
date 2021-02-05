open Common

let f (g, a, b, c, d, e) = g a b c d e

let sync_set boolref mutex =
  Mutex.lock mutex;
  boolref := not !boolref;
  Mutex.unlock mutex

let rec mine block pair difficulty current_id mutex =
  let found = !current_id > block.id in
  if found then None
  else
    let hash = block_fingerprint block in
    if hash_is_solution hash difficulty then Some (block.nonce, hash)
    else
      mine
        { block with nonce = block.nonce + 2 }
        pair difficulty current_id mutex

(*
  Hypothèse : dans la liste, les id des blocks sont la suite naturelle 0..n
*)
let mine_blocklist_thread blocklist pair difficulty current_id mutex =
  let my_name = if pair then " pair " else "impair" in
  let other_name = if pair then "impair" else " pair " in
  let first = ref false in

  let rec loop blocklist =
    match blocklist with
    | b :: rest ->
        let block = if not pair then { b with nonce = 1 } else b in

        (match mine block pair difficulty current_id mutex with
        | Some (nonce, hash) ->
            Mutex.lock mutex;
            if !current_id > block.id then first := false
            else (
              current_id := !current_id + 1;
              first := true);
            Mutex.unlock mutex;
            block.nonce <- nonce
        | None -> first := false);

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
  let current_id = ref 0 in
  let m = Mutex.create () in
  let thread_pair =
    Thread.create f
      (mine_blocklist_thread, blocklist, true, difficulty, current_id, m)
  in
  let thread_impair =
    Thread.create f
      (mine_blocklist_thread, blocklist, false, difficulty, current_id, m)
  in
  Thread.join thread_pair;
  Thread.join thread_impair

let () =
  print_endline "Toy program to test mining in two parallel threads";

  let n = 5 in
  let blocklist = make_block_list n in
  (*
     ==V== TO CHANGE DIFFICULTY ==V==
  *)
  let difficulty = 3 in
  mine_blocklist blocklist difficulty;
  (*let _ = Thread.create f (mine_blocklist, blocklist, false, 1) in*)
  ignore (read_line ())
