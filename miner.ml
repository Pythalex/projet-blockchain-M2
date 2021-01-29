open Unix
open Common

(* -- network variables -- *)

let server_ip = inet_addr_loopback

let server_port = ref 0

let connect_to_ip = ref server_ip

let connect_to_port = ref 0

let set_connect_to_ip ip = connect_to_ip := inet_addr_of_string ip

let speclist =
  [
    ("-p", Arg.Set_int server_port, "Listening port number");
    ("--remote-ip", Arg.String set_connect_to_ip, "Remote miner's ip");
    ("--remote-port", Arg.Set_int connect_to_port, "Remote miner's port number");
  ]

let usage_msg = "Super bitcoin miner"

;;
Arg.parse speclist print_endline usage_msg

(* -- application variables -- *)

let mynodetype = Miner

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

let main () =
  (* command argument check -> listening port is required *)
  if !server_port = 0 then (
    Arg.usage speclist usage_msg;
    exit 0 );

  (* Listening socket *)
  let addr = ADDR_INET (server_ip, !server_port) in
  let s = socket PF_INET SOCK_STREAM 0 in

  (* Call miner if a port was given in argument *)
  if !connect_to_port <> 0 then (
    network :=
      connect_to_miner Miner server_ip !server_port !connect_to_ip
        !connect_to_port;
    Printf.printf "Réponse reçue de la part du miner distant\n%!";
    print_NodeSet !network );

  (* Option pour que le socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;

  bind s addr;
  Printf.printf "Listening on port %d...\n%!" !server_port;
  listen s 5;

  (* Main loop *)
  while true do
    (* client socket *)
    let client_socket, client_addr = accept s in
    let client_ip, client_port = extract_ip_port_from_sockaddr client_addr in

    let in_chan = in_channel_of_descr client_socket in
    let out_chan = out_channel_of_descr client_socket in

    let input_message = input_value in_chan in

    (* ignore duplicated messages *)
    if already_received_message !received_messages input_message then
      print_endline "Ignoring duplicated message"
    else (
      (* Else treat new message accordingly *)
      received_messages := add_message !received_messages input_message;

      Printf.printf "Received %s from %s:%d.\n%!"
        (message_literal input_message)
        (string_of_inet_addr client_ip)
        client_port;

      (* Try to understand the message *)
      match input_message with
      (* A new node asks to enter the network *)
      | Greetings (nodetype, ip, port) ->
          let _a = Hashtbl.hash input_message in

          (* Gives the network map to the new node *)
          Printf.printf "Greetings new %s.\n%!" (nodetype_literal nodetype);
          greet_new_node !network mynodetype server_ip !server_port out_chan;

          (* Broadcast the new node id *)
          print_endline "Sharing new node to rest of the network.";
          share_new_node !network (nodetype, ip, port);

          (* Update own network map *)
          network := NodeSet.add (nodetype, ip, port) !network;
          print_new_network !network
      (* A new node has been registered in the network *)
      | NetworkNewNode (nodetype, ip, port) ->
          Printf.printf "Received new node of type %s.\n%!"
            (nodetype_literal nodetype);

          (* Update own network map *)
          network := NodeSet.add (nodetype, ip, port) !network;
          print_new_network !network
      | _ -> () )
  done

let () = main ()