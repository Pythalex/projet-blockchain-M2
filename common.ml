open Unix

exception NotUnderstood of string

exception NotImplemented


let get_timestamp () =
  let t = time () in
  t

(*
  Possible types of nodes in the network
*)
type nodetype = Miner | Wallet

(*
  Stringify of node types
*)
let nodetype_literal nt = match nt with Miner -> "Miner" | Wallet -> "Wallet"

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

(*
  Function: string_of_sockaddr
  Get the string representation of a socket address as 'ip:port'
*)
let string_of_sockaddr sockaddr =
  let (ip, port) = extract_ip_port_from_sockaddr sockaddr in
  Printf.sprintf "%s:%d" (string_of_inet_addr ip) port


(*
  Module: NodeSet
  A custom set containing nodes as tuple (nodetype, ip, port)
  Sort nodes by comparing their port number
*)
module NodeSet = Set.Make (struct
  let compare sockaddr1 sockaddr2 =
    let (ip1, port1) = extract_ip_port_from_sockaddr sockaddr1 in
    let (ip2, port2) = extract_ip_port_from_sockaddr sockaddr2 in
    Stdlib.compare port1 port2
    
  type t = Unix.sockaddr
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
let print_node addr =
  Printf.printf "\t(%s)\n" (string_of_sockaddr addr)

(*
  Function: print_NodeSet
  Print a NodeSet custom set
*)
let print_NodeSet set = print_set print_node set

(*
  Type enregistrement 
*)
type transaction = {
  id : int;
  source : string;
  destination : string;
  amount : float;
}

let make_transaction s d a =
  {id = int_of_float (1000. *. get_timestamp ()); source = s; destination = d; amount = a}

let string_of_transaction transaction =
  Format.sprintf "Transaction(id=%d) from '%s' to '%s' : %f%!" transaction.id transaction.source transaction.destination transaction.amount

(*
  Abstract representation of a blockchain block
*)
type block = {
  (* header *)
  id : int;
  nonce : int;
  prevhash : string;
  hash : string;
  
  (* payload *)
  transactions : transaction list;
}

(*
  Function: make_block
  Creates a block with given message and id and nonce 0.
*)
let make_block id transactions prevhash =
  { id = id; 
  nonce = 0; 
  prevhash = prevhash; 
  hash = "";
  transactions = transactions}

let genesis = make_block 0 [] ""

(*
  Function: make_block_list
  Creates a list of n block with id starting from 0 to n - 1.
*)
let make_block_list n =
  List.init n (fun i -> make_block i [])

(*
  Function: block_fingerprint
  Returns the md5 fingerprint of a block
*)
let block_fingerprint block =
  Digest.string (Marshal.to_string block [])

let blockchain_last_block blockchain =
  match blockchain with
    b::r -> b
    | [] -> genesis

let blockchain_previous_id blockchain =
  (* last block = first in chained list - stack -*)
  match blockchain with
    | x::b -> x.id
    | _ -> 0

let blockchain_previous_hash blockchain =
  match blockchain with
    | x::b -> x.hash
    | _ -> ""

let string_of_block block =
  let buffer = Buffer.create 42 in
  Buffer.add_string buffer (Format.sprintf "Block(ID = %d):\n    nonce = %d\n    transactions = {\n" block.id block.nonce);
  let f trans = Buffer.add_string buffer ("        " ^ string_of_transaction trans ^ "\n") in
  List.iter f block.transactions;
  Buffer.add_string buffer "    }";
  Buffer.contents buffer (* returns the final string *)


(*
  Function: hash_is_solution
  Indicates if the given hash starts with '0' * difficulty,
  or 'difficulty' times the '0' character
*)
let hash_is_solution hash difficulty =
  let hash_start = String.sub hash 0 difficulty in
  hash_start = String.make difficulty '0'
 
(*
  All types of messages used for communication between nodes in the network
*)
type message =
  | Greetings of sockaddr
  | NetworkMap of NodeSet.t
  | NetworkNewNode of sockaddr
  | Block of block
  | Blockchain of block list
  | BlockchainHeader of block list
  | TransactionExist 
  | TransactionWaiting
  | TransactionNotExist
  | ShowBlockchain
  | ShowPeers
  | Transaction of transaction
  | Confirmation of transaction

(*
  Stringify of node communication messages
*)
let message_literal msg =
  match msg with
  | Greetings _ -> "Greetings"
  | NetworkMap _ -> "NetworkMap"
  | NetworkNewNode _ -> "NetworkNewNode"
  | Block _ -> "Block"
  | Blockchain _ -> "Blockchain"
  | BlockchainHeader _ -> "BlockchainHeader"
  | TransactionExist -> "TransactionExist"
  | TransactionWaiting -> "TransactionWaiting"
  | TransactionNotExist -> "TransactionNotExist"
  | ShowBlockchain -> "ShowBlockchain"
  | ShowPeers -> "ShowPeers"
  | Transaction _ -> "Transaction"
  | Confirmation _ -> "Confirmation"

(*
  Function: add_message
  Adds the message to the set of received messages and returns the new set  
*)
let add_message received_messages msg =
  let hash = Hashtbl.hash (Marshal.to_string msg []) in
  HashSet.add hash received_messages

(*
  Function: already_received_message
  Indicates if the given message exists in the hash table of received messages
*)
let already_received_message received_messages msg =
  let hash_msg = Hashtbl.hash (Marshal.to_string msg []) in
  let is_msg m = m = hash_msg in
  HashSet.exists is_msg received_messages


(*
    Function: broadcast
    Broadcast a message to the given network.

    Arguments: 
      network, the node set
      msg, the object to send

    Returns: the network without dead nodes
*)
let broadcast network msg =
  let didnt_respond = ref NodeSet.empty in

  let share addr =
    let s = socket PF_INET SOCK_STREAM 0 in

    (*Printf.printf "Broadcasting to miner@%s.\n%!" (string_of_sockaddr addr);*)
 
    (* Try to broadcast to the node, remember if it doesn't respond *)
    ( try
        connect s addr;
        let out_chan = out_channel_of_descr s in
        output_value out_chan msg;
        flush out_chan
      with Unix_error (Unix.ECONNREFUSED, _, _) ->
        didnt_respond := NodeSet.add addr !didnt_respond );

    Unix.close s
  in

  NodeSet.iter share network;
  NodeSet.diff network !didnt_respond



(*
  Function: connect_to_miner
  Connect to miner at given address and return miner network 

  Arguments:
    nodetype, the node type (miner | wallet)
    my_ip, the current listening process ip
    my_port, the current listening process port number
    miner_ip, the remote miner ip
    miner_port, the remote miner port

  Returns:

*)
let connect_to_miner my_address miner_address =
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s miner_address;

  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in
  Printf.printf "Connected to miner at %s.\n%!" (string_of_sockaddr miner_address);

  (* self identification to remote node  *)
  output_value out_chan (Greetings (my_address));
  flush out_chan;
  print_endline "Sent identification.";

  (* receive network map *)
  match input_value in_chan with
  | NetworkMap n ->
      Unix.close s;
      n
  | _ -> raise (NotUnderstood "Expected NetworkMap.")

(*
  Function: get_peers
  Get the node network from given miner but doesn't enter the network.
  Used by wallets.
*)
let show_peers miner_address =
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s miner_address;

  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in
  Printf.printf "Connected to miner at %s.\n%!" (string_of_sockaddr miner_address);

  (* self identification to remote node  *)
  output_value out_chan ShowPeers;
  flush out_chan;
  print_endline "Asking network map.";

  (* receive network map *)
  match input_value in_chan with
  | NetworkMap n ->
      Unix.close s;
      n
  | _ -> raise (NotUnderstood "Expected NetworkMap.")
