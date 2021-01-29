open Unix

exception NotUnderstood of string

exception NotImplemented

(*
  Possible types of nodes in the network
*)
type nodetype = Miner | Wallet

(*
  Stringify of node types
*)
let nodetype_literal nt = match nt with Miner -> "Miner" | Wallet -> "Wallet"

(*
  Module: NodeSet
  A custom set containing nodes as tuple (nodetype, ip, port)
  Sort nodes by comparing their port number
*)
module NodeSet = Set.Make (struct
  let compare (nodetype, ip1, port1) (nodetype, ip2, port2) =
    Stdlib.compare port1 port2

  type t = nodetype * Unix.inet_addr * int
end)

module HashSet = Set.Make (struct
  let compare = Stdlib.compare

  type t = int
end)

(*
  Function: print_set
  Prints a custom set given an element wise print function
  
  Arguments: 
    print_element, the function that can print individual element of the set
    set, the set to print
  Returns: unit
*)
let print_set print_element set =
  let printIter elt = print_element elt in
  print_string "{\n";
  NodeSet.iter printIter set;
  print_string "}\n";
  flush Stdlib.stdout

(*
  Function: print_node
  Prints a network node. A node is given by (Nodetype, IP, port number)

  Arguments:
    nodetype, union type Miner | Wallet
    ip, INET_ADDR the ip address of the node in the network
    port, int the listening port number of the node
*)
let print_node (nodetype, ip, port) =
  Printf.printf "\t(%s, %s, %d)\n"
    (nodetype_literal nodetype)
    (Unix.string_of_inet_addr ip)
    port

(*
  Function: print_NodeSet
  Print a NodeSet custom set
*)
let print_NodeSet set = print_set print_node set

(*
  All types of messages used for communication between nodes in the network
*)
type message =
  | Greetings of nodetype * inet_addr * int
  | NetworkMap of NodeSet.t
  | NetworkNewNode of nodetype * inet_addr * int

(*
  Stringify of node communication messages
*)
let message_literal msg =
  match msg with
  | Greetings _ -> "Greetings"
  | NetworkMap _ -> "NetworkMap"
  | NetworkNewNode _ -> "NetworkNewNode"

(*
  Function: add_message
  Adds the message to the set of received messages and returns the new set  
*)
let add_message received_messages msg =
  HashSet.add (Hashtbl.hash msg) received_messages

(*
  Function: already_received_message
  Indicates if the given message exists in the hash table of received messages
*)
let already_received_message received_messages msg =
  let hash_msg = Hashtbl.hash msg in
  let is_msg m = m = hash_msg in
  HashSet.exists is_msg received_messages

(*
  Function extract_ip_port_from_sockaddr:
  Get the information contained in a socket, the union type sockaddr objet

  Arguments: sockaddr, the socket 
  Returns: pair (ip, port) of the socket

  Remark : str to ip port not implemented
*)
let extract_ip_port_from_sockaddr sockaddr =
  match sockaddr with
  | ADDR_UNIX str -> raise NotImplemented
  | ADDR_INET (ip, port) -> (ip, port)

(**
  Filter given network (node set) and returns only miners
*)
let only_miner_filter network =
  let is_miner (nodetype, ip, port) =
    match nodetype with Miner -> true | Wallet -> false
  in
  let filtered = NodeSet.filter is_miner network in
  print_NodeSet filtered;
  filtered

(**
  Filter given network (node set) and returns only miners
*)
let only_wallet_filter network =
  let is_wallet (nodetype, ip, port) =
    match nodetype with Miner -> false | Wallet -> true
  in
  NodeSet.filter is_wallet network

let broadcast network msg =
  let share (nodetype, ip, port) =
    let addr = ADDR_INET (ip, port) in
    let s = socket PF_INET SOCK_STREAM 0 in

    Printf.printf "Broadcasting to %s@%s:%d.\n%!"
      (nodetype_literal nodetype)
      (string_of_inet_addr ip) port;

    connect s addr;

    let out_chan = out_channel_of_descr s in

    output_value out_chan msg;
    flush out_chan;

    Unix.close s
  in

  NodeSet.iter share network

let share_new_node network (nodetype, new_ip, new_port) =
  broadcast (only_miner_filter network) (nodetype, new_ip, new_port)

(** Connect to miner at given address and return miner network *)
let connect_to_miner nodetype my_ip my_port miner_ip miner_port =
  let addr = ADDR_INET (miner_ip, miner_port) in
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s addr;

  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in
  Printf.printf "Connected to miner at %s:%d.\n%!"
    (string_of_inet_addr miner_ip)
    miner_port;

  (* self identification to remote node  *)
  output_value out_chan (Greetings (nodetype, my_ip, my_port));
  flush out_chan;
  print_endline "Sent identification.";

  (* receive network map *)
  match input_value in_chan with
  | NetworkMap n ->
      Unix.close s;
      n
  | _ -> raise (NotUnderstood "Expected NetworkMap.")

(* 
  Function: greet_new_node
  Send the network map to the new node
*)
let greet_new_node network nodetype server_ip server_port out_chan =
  output_value out_chan
    (NetworkMap (NodeSet.add (nodetype, server_ip, server_port) network));
  flush out_chan
