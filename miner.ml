open Unix
open Common

(* -- network variables -- *)
let name = ref ""

let server_ip = inet_addr_loopback

let server_port = ref 0

let connect_to_ip = ref server_ip

let connect_to_port = ref 0

let set_connect_to_ip ip = connect_to_ip := inet_addr_of_string ip

let speclist =
  [
    ("-p", Arg.Set_int server_port, "Listening port number");
    ("--ri", Arg.String set_connect_to_ip, "Remote miner's ip");
    ("--rp", Arg.Set_int connect_to_port, "Remote miner's port number");
  ]

let usage_msg = "Super bitcoin miner"

;;
Arg.parse speclist print_endline usage_msg

(* -- application variables -- *)
(*
  Contains known nodes in the network
*)
let network = ref NodeSet.empty

let blockchain = [ genesis ]

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

let mining () = ()

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

let receive_transaction transaction =
  print_endline "Adding transaction to current block and broadcast it";

  Mutex.lock mining_block_mutex;
  (match !in_mining_block with
  | Some block -> block.transactions <- transaction :: block.transactions
  | None ->
      in_mining_block := Some (make_block (last_blockchain_id blockchain) []));
  Mutex.unlock mining_block_mutex;

  let message = Transaction transaction in
  received_messages := add_message !received_messages message;
  network := broadcast !network message

(*
  Function process_client
  Processes incoming message.
*)
let process_client my_address client_socket client_addr =
  let client_ip, client_port = extract_ip_port_from_sockaddr client_addr in

  let in_chan = in_channel_of_descr client_socket in
  let out_chan = out_channel_of_descr client_socket in

  let input_message = input_value in_chan in

  Printf.printf "Received %s from %s:%d.\n%!"
    (message_literal input_message)
    (string_of_inet_addr client_ip)
    client_port;

  (* Try to understand the message *)
  match input_message with
  (* A new node asks to enter the network *)
  | Greetings miner_addr ->
      Printf.printf "Greetings new miner listening at %s\n%!"
        (string_of_sockaddr miner_addr);

      (* Gives the network map to the new node *)
      greet_new_node my_address out_chan;

      (* Broadcast the new node id *)
      print_endline "Sharing new node to rest of the network.";
      share_new_node miner_addr;

      (* Update own network map *)
      network := NodeSet.add miner_addr !network;
      print_new_network !network
  (* A new node has been registered in the network *)
  | NetworkNewNode miner_addr ->
      if already_received_message !received_messages input_message then
        print_endline "Ignoring duplicated message"
      else (
        received_messages := add_message !received_messages input_message;
        Printf.printf "Received new miner listening at %s.\n%!"
          (string_of_sockaddr miner_addr);

        print_endline "Broadcasting new node";
        share_new_node miner_addr;

        (* Update own network map *)
        network := NodeSet.add miner_addr !network;
        print_new_network !network)
  | ShowPeers ->
      output_value out_chan (NetworkMap (NodeSet.add my_address !network));
      flush out_chan
  | Transaction t ->
      if already_received_message !received_messages input_message then
        print_endline "Ignoring duplicated message"
      else (
        Printf.printf "Received %s.\n%!" (string_of_transaction t);
        receive_transaction t)
  | _ -> print_endline "I don't understand the message, ignoring."

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
    print_endline "--------------------------------";

    (* client socket *)
    let client_socket, client_addr = accept s in
    let f (g, a, b, c) = g a b c in
    let _ =
      Thread.create f (process_client, my_address, client_socket, client_addr)
    in
    ()
  done

let () = main ()
