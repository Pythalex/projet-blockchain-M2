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

let mining () = print_endline "Started miner ..."

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
      greet_new_node !network my_address out_chan;

      (* Broadcast the new node id *)
      print_endline "Sharing new node to rest of the network.";
      let didnt_respond = share_new_node !network miner_addr in
      (* mark own message as seen to ignore it when it echoes *)
      received_messages :=
        add_message !received_messages (NetworkNewNode miner_addr);

      (* Update own network map *)
      (* remove dead nodes *)
      network := NodeSet.diff !network didnt_respond;
      (* add new *)
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
        let didnt_respond = share_new_node !network miner_addr in

        (* Update own network map *)
        (* remove dead nodes *)
        network := NodeSet.diff !network didnt_respond;
        (* add new *)
        network := NodeSet.add miner_addr !network;
        print_new_network !network)
  | ShowPeers ->
      output_value out_chan (NetworkMap (NodeSet.add my_address !network));
      flush out_chan
  | Transaction t ->
      Printf.printf "Received transaction of %f bitconneeeeect from %s to %s.\n%!" t.amount t.source t.destination
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
