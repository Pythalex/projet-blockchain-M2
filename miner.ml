open Unix
open IPPortSet

(* network variables *)
let server_ip = inet_addr_loopback
let server_port = ref 0
let connect_to_ip = ref server_ip
let connect_to_port = ref 0

let set_connect_to_ip ip = connect_to_ip := inet_addr_of_string ip

let speclist = [
  ("-p", Arg.Set_int server_port, "Listening port number");
  ("--remote-ip", Arg.String set_connect_to_ip, "Remote miner's ip");
  ("--remote-port", Arg.Set_int connect_to_port, "Remote miner's port number");
]

let usage_msg = "Super bitcoin miner";;

Arg.parse speclist print_endline usage_msg;;

(* application variables *)
let miner_network = ref IPPortSet.empty

type message = 
  | GreetingsMiner of inet_addr * int
  | NetworkMap of IPPortSet.t
  | NetworkNewNode of inet_addr * int
  | NetworkDeletedNode of inet_addr * int

let message_literal msg =
  match msg with
    GreetingsMiner _ -> "GreetingsMiner"
    | NetworkMap _ -> "NetworkMap"
    | NetworkNewNode _ -> "NetworkNewNode"
    | NetworkDeletedNode _ -> "NetworkDeletedNode"

exception NotUnderstood of string
exception NotImplemented


let extract_ip_port_from_sockaddr sockaddr =
  match sockaddr with
    | ADDR_UNIX (str) -> raise NotImplemented
    | ADDR_INET (ip, port) -> (ip, port)


(** Connect to miner at given address and return miner network *)
let connect_to_miner my_ip my_port miner_ip miner_port =
  let addr = ADDR_INET(miner_ip, miner_port) in
  let s = socket PF_INET SOCK_STREAM  0 in
  connect s addr;

  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in
  Printf.printf "Connected to miner at %s:%d.\n%!" (string_of_inet_addr miner_ip) miner_port;

  (* self identification to remote miner *)
  output_value out_chan (GreetingsMiner (my_ip, my_port));
  flush out_chan;
  print_endline "Sent identification.";

  (* receive network map *)
  match input_value in_chan with
    NetworkMap n -> 
      Unix.close s; 
      n
    | _ -> raise (NotUnderstood "Expected NetworkMap.")


let greet_new_miner miner_network server_ip server_port out_chan =
  output_value out_chan (NetworkMap (IPPortSet.add (server_ip, server_port) miner_network));
  flush out_chan


let share_new_miner miner_network new_ip new_port =
  let share (to_ip, to_port) =
    (* don't share new node info with itself *)
    if not (to_ip = new_ip && to_port = new_port) then begin
      let addr = ADDR_INET(to_ip, to_port) in
      let s = socket PF_INET SOCK_STREAM  0 in

      connect s addr;

      let out_chan = out_channel_of_descr s in

      output_value out_chan (NetworkNewNode (new_ip, new_port));
      flush out_chan;

      Printf.printf "Shared new node with %s:%d.\n%!" 
        (string_of_inet_addr to_ip) to_port;

      Unix.close s;
    end
  in

  IPPortSet.iter share miner_network


let main () =

  (* command argument check *)
  if !server_port = 0 then 
  begin
    Arg.usage speclist usage_msg;
    exit 0
  end;

  let addr = ADDR_INET(server_ip, !server_port) in
  let s = socket PF_INET SOCK_STREAM 0 in

  (* Call miner if a port was given in argument *)
  if !connect_to_port <> 0 then 
  begin
    miner_network := connect_to_miner server_ip !server_port 
      !connect_to_ip !connect_to_port;
    Printf.printf "Réponse reçue de la part du miner distant\n%!";
    printSet !miner_network;
  end;

  (* Option pour que la socket soit réutilisable *)
  setsockopt s SO_REUSEADDR true;

  bind s addr;

  Printf.printf "Listening on port %d...\n%!" !server_port;
  listen s 5;

  while true do
    let client_socket, client_addr = accept s in
    let (client_ip, client_port) = extract_ip_port_from_sockaddr client_addr in
    
    let in_chan = in_channel_of_descr client_socket in
    let out_chan = out_channel_of_descr client_socket in

    let input_message = input_value in_chan in
    Printf.printf "Received %s from %s:%d.\n%!" (message_literal input_message) 
      (string_of_inet_addr client_ip) client_port;

    (match input_message with
      GreetingsMiner (ip, port) -> 
          print_endline "Greetings new miner.";
          greet_new_miner !miner_network server_ip !server_port out_chan;
          miner_network := IPPortSet.add (ip, port) !miner_network;

          print_endline "Sharing new node to rest of the network.";
          share_new_miner !miner_network ip port

      | NetworkNewNode (ip, port) ->
          print_endline "Received new node.";
          miner_network := IPPortSet.add (ip, port) !miner_network

      | _ -> ()
    );

    print_endline "New network:";
    printSet !miner_network;
    
  done


let () = 
  main ()