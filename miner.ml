open Unix
open Common

exception AlreadyMined

exception NewTransaction

exception EarlyStop

(* -- network variables -- *)
let name = ref ""

let server_ip = inet_addr_loopback

let server_port = ref 0

let connect_to_ip = ref server_ip

let connect_to_port = ref 0

let set_connect_to_ip ip = connect_to_ip := inet_addr_of_string ip

let difficulty = ref 3

let speclist =
  [
    ("-p", Arg.Set_int server_port, "Listening port number [REQUIRED]");
    ("--ri", Arg.String set_connect_to_ip, "Remote miner's ip");
    ("--rp", Arg.Set_int connect_to_port, "Remote miner's port number");
    ("-d", Arg.Set_int difficulty, "Mining difficulty (default 3)");
  ]

let usage_msg = "Super bitcoin miner"

;;
Arg.parse speclist print_endline usage_msg

(* -- application variables -- *)
(*
  Contains known nodes in the network
*)
let network = ref NodeSet.empty

let network_mutex = Mutex.create ()

let blockchain = ref [ genesis ]

let blockchain_mutex = Mutex.create ()

let in_mining_block = ref None

let mining_block_mutex = Mutex.create ()

(*
  Will contain hash of every received messages
*)
let received_messages = ref HashSet.empty

(*
  Prints the node network
*)
let print_new_network network =
  print_endline "New network:";
  print_NodeSet network

(*
  Function: puzzle
  Returns a nonce such as hash (block nonce) matches difficulty
*)
let rec puzzle block =
  if blockchain_previous_id !blockchain >= block.id then raise AlreadyMined
  else (
    Mutex.lock mining_block_mutex;
    (match !in_mining_block with
    | Some b ->
        if b.transactions <> block.transactions then (
          Mutex.unlock mining_block_mutex;
          raise NewTransaction)
        else ()
    | None -> raise EarlyStop);
    Mutex.unlock mining_block_mutex);
  let hash = block_fingerprint block in
  if hash_is_solution hash !difficulty then block.nonce
  else puzzle { block with nonce = block.nonce + 1 }

(*
  Function: mining
  Main mining thread function
  
  Loops infinitely and mines whenever in_mining_block contains a block.

*)
let mining () =
  while true do
    (* sleep 0.5 s *)
    Thread.delay 0.5;

    Mutex.lock mining_block_mutex;
    let mb = !in_mining_block in
    Mutex.unlock mining_block_mutex;

    try
      match mb with
      | Some b ->
          print_endline "start mining";
          let block_to_mine = {b with hash = block_fingerprint b} in

          let nonce = puzzle block_to_mine in
          let block_mined = {block_to_mine with nonce = nonce} in
          Printf.printf "Found nonce = %d\n%!" nonce;

          Mutex.lock blockchain_mutex;
          Mutex.lock network_mutex;
          Mutex.lock mining_block_mutex;

          blockchain := block_mined :: !blockchain;
          network := broadcast !network (Block block_mined);
          in_mining_block := None;

          Mutex.unlock mining_block_mutex;
          Mutex.unlock network_mutex;
          Mutex.unlock blockchain_mutex;
          print_endline (string_of_block (blockchain_last_block !blockchain))
      | None -> ()
    with
    | AlreadyMined ->
        print_endline "Other miner found nonce.";
        Mutex.lock mining_block_mutex;
        in_mining_block := None;
        Mutex.unlock mining_block_mutex;
        print_endline (string_of_block (blockchain_last_block !blockchain))
    | NewTransaction ->
        print_endline "New transaction arrived, start mining again..."
    | EarlyStop -> print_endline "Mining stopped early."
  done

(* 
  Function: greet_new_node
  Send the network map to the new node
*)
let greet_new_node my_address out_chan =
  output_value out_chan (NetworkMap (NodeSet.add my_address !network));
  flush out_chan

(*
  Function: share_new_node
  Broadcast the node info to the miners of the network.
  Wallets are not contacted.

  Arguments: 
    network, the node set
    (nodetype, new_ip, new_port), the new node info
*)
let share_new_node addr =
  let message = NetworkNewNode addr in
  received_messages := add_message !received_messages message;
  network := broadcast !network message

(* 
  Function: receive_transaction
  Add the given transaction in the block to be mined and broadcast it
  among the network.

  Arguments:
    t, the wallet transaction
*)
let receive_transaction t =
  print_endline "Adding transaction to current block and broadcast it";

  Mutex.lock mining_block_mutex;
  (match !in_mining_block with
  | Some block ->
      let new_block = make_block block.id (t :: block.transactions) block.prevhash in
      in_mining_block := Some new_block;
      print_endline (string_of_block new_block)
  | None ->
      in_mining_block :=
        Some
          (make_block
             (blockchain_previous_id !blockchain + 1)
             [ t ]
             (blockchain_previous_hash !blockchain)));
  Mutex.unlock mining_block_mutex;

  let message = Transaction t in
  received_messages := add_message !received_messages message;
  network := broadcast !network message

(*
  Function: look_for_transaction_in_mining_block
  Indicates if the given transaction hash is found in the transaction
  list of the block being mined.

  Arguments:
    thash, hash of a transaction object
    lock, whether to use the mutex lock on the mining block
*)
let look_for_transaction_in_mining_block thash lock =
  let res = ref false in
  if lock then
    Mutex.lock mining_block_mutex;
  (match !in_mining_block with
    | Some block ->
        if List.exists (fun t -> hash t = thash) block.transactions then
          res := true
        else
          res := false
    | None ->
      res := false);
  if lock then
    Mutex.unlock mining_block_mutex;
  !res

(*
  Function: get_mining_block
  Get the mining block contained in the Option type object representing
  the mining block. 
*)
let get_mining_block () =
  match !in_mining_block with
    Some b -> b
    | None -> raise Not_found

(*
  Function: find_transaction_index
  Returns the transaction index of the transaction given
  its hash in the given transaction list.

  Arguments:
    transactions, the list of transactions
    thash, the hash of the transaction to look for
*)
let find_transaction_index transactions thash =
  let rec loop transactions t i =
    match transactions with
      | [] -> raise Not_found
      | t::l -> 
        (* This is used in order to make ocaml map the parameters 
        to transaction record type instead of the block record type *) 
        if hash t = thash then i else loop l thash i+1
  in
  loop transactions thash 0

(*
  Function: confirm_transaction
  Returns a merkle proof for the given transaction if it
  exists in a block of the blockchain.

  Arguments:
    thash, hash of the transaction to confirm
    out_chan, out communication channel to the wallet

  Note:
    returns:
    - TransactionNotExist if the transaction hash is not found in blockchain
    - TransactionWaiting if the transaction is in the block being currently mined
    - TransactionExist (proof, block_id) if the transaction is found in a block
*)
let confirm_transaction thash out_chan =
  
  let block = ref genesis in
  let inwaiting = ref false in
  let found = ref false in
  
  (try
    (Mutex.lock mining_block_mutex;
    if look_for_transaction_in_mining_block thash false then
      (inwaiting := true;
      found := true)
    else
      (block := find_block_by_transaction !blockchain thash;
      found := true));
    Mutex.unlock mining_block_mutex
  with
    Not_found ->(
      found := false));

  if !inwaiting then
    (output_value out_chan TransactionWaiting)
  else if not !found then
    (output_value out_chan TransactionNotExist)
  else
    (print_endline "Creating proof";
    let id = !block.id in
    let t_idx = find_transaction_index (!block.transactions) thash in
    let t_hash = List.map hash !block.transactions in
    let tree = Merkle.make t_hash in
    let proof = Merkle.proof tree t_idx in
    output_value out_chan (TransactionExist (proof, id)))
  ;
  flush out_chan
  

(*
  Function process_client
  Processes incoming message.
*)
let process_client my_address client_socket client_addr =
  (*let client_ip, client_port = extract_ip_port_from_sockaddr client_addr in*)
  let in_chan = in_channel_of_descr client_socket in
  let out_chan = out_channel_of_descr client_socket in

  let input_message = input_value in_chan in

  (*Printf.printf "Received %s from %s:%d.\n%!"
    (message_literal input_message)
    (string_of_inet_addr client_ip)
    client_port;*)

  (* Try to understand the message *)
  match input_message with
  (* A new node asks to enter the network *)
  | Greetings miner_addr ->
      Printf.printf "Greetings new miner listening at %s\n%!"
        (string_of_sockaddr miner_addr);

      (* Gives the network map to the new node *)
      greet_new_node my_address out_chan;

      (* Broadcast the new node id *)
      (*print_endline "Sharing new node to rest of the network.";*)
      share_new_node miner_addr;

      (* Update own network map *)
      network := NodeSet.add miner_addr !network;
      print_new_network !network
  (* A new node has been registered in the network *)
  | NetworkNewNode miner_addr ->
      if already_received_message !received_messages input_message then
        (*print_endline "Ignoring duplicated message"*)
        ()
      else (
        received_messages := add_message !received_messages input_message;
        Printf.printf "Received new miner listening at %s.\n%!"
          (string_of_sockaddr miner_addr);

        (*print_endline "Broadcasting new node";*)
        share_new_node miner_addr;

        (* Update own network map *)
        network := NodeSet.add miner_addr !network;
        print_new_network !network)
  | ShowPeers ->
      output_value out_chan (NetworkMap (NodeSet.add my_address !network));
      flush out_chan
  | ShowBlockchainHeader ->
      output_value out_chan (BlockchainHeader (headers_of_blockchain !blockchain));
      flush out_chan
  | Transaction t ->
      if already_received_message !received_messages input_message then
        (*print_endline "Ignoring duplicated message"*)
        ()
      else (
        received_messages := add_message !received_messages input_message;
        Printf.printf "Received %s.\n%!" (string_of_transaction t);
        receive_transaction t)
  | Confirmation thash ->
      Printf.printf "Received confirmation request for transaction of hash = %s.\n%!" thash;
      confirm_transaction thash out_chan
  | Block b ->
    if already_received_message !received_messages input_message then
      (*print_endline "Received duplicated block message broadcast, ignoring."*)
      ()
    else (
      received_messages := add_message !received_messages input_message;
      Mutex.lock blockchain_mutex;
      if b.id > blockchain_previous_id !blockchain then (
        blockchain := b :: !blockchain;
        print_endline "Received block and added it into blockchain")
      else print_endline "Received block with invalid id, ignoring it";
      Mutex.unlock blockchain_mutex;

      network := broadcast !network input_message)
  | _ ->
      (*print_endline "I don't understand the message, ignoring."*)
      ()

let main () =
  (* command argument check -> listening port is required *)
  if !server_port = 0 then (
    Arg.usage speclist usage_msg;
    exit 0);

  (* Listening socket *)
  let my_address = ADDR_INET (server_ip, !server_port) in
  let s = socket PF_INET SOCK_STREAM 0 in

  (* Call miner if a port was given in argument *)
  if !connect_to_port <> 0 then (
    let miner_address = ADDR_INET (!connect_to_ip, !connect_to_port) in
    network := connect_to_miner my_address miner_address;
    Printf.printf "Réponse reçue de la part du miner distant\n%!";
    print_NodeSet !network);

  (* Option pour que le socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;

  bind s my_address;
  Printf.printf "Listening on port %d...\n%!" !server_port;
  listen s 5;

  let _ = Thread.create mining () in

  (* Main loop *)
  while true do
    (* client socket *)
    let client_socket, client_addr = accept s in
    let f (g, a, b, c) = g a b c in
    let _ =
      Thread.create f (process_client, my_address, client_socket, client_addr)
    in
    ()
  done

let () = main ()
