open Unix

exception NotUnderstood of string

exception NotImplemented

type nodetype = Miner | Wallet

let nodetype_literal nt = match nt with Miner -> "Miner" | Wallet -> "Wallet"

module NodeSet = Set.Make (struct
  let compare (nodetype, ip1, port1) (nodetype, ip2, port2) =
    Stdlib.compare port1 port2

  type t = nodetype * Unix.inet_addr * int
end)

let printPair (nodetype, ip, port) =
  Printf.printf "\t(%s, %s, %d)\n"
    (nodetype_literal nodetype)
    (Unix.string_of_inet_addr ip)
    port

let printSet set =
  let printIter elt = printPair elt in

  print_string "{\n";
  NodeSet.iter printIter set;
  print_string "}\n";
  flush Stdlib.stdout

type message =
  | Greetings of nodetype * inet_addr * int
  | NetworkMap of NodeSet.t
  | NetworkNewNode of nodetype * inet_addr * int

let message_literal msg =
  match msg with
  | Greetings _ -> "Greetings"
  | NetworkMap _ -> "NetworkMap"
  | NetworkNewNode _ -> "NetworkNewNode"

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

let disconnect_from_network miner_network server_ip server_port =
  print_endline
    "\n\
     To Implement : multi-thread with clean listening thread closing from this \
     function."

let share_new_node network new_ip new_port =
  let share (nodetype, to_ip, to_port) =
    (* don't share new node info with itself *)
    if not (to_ip = new_ip && to_port = new_port) then (
      let addr = ADDR_INET (to_ip, to_port) in
      let s = socket PF_INET SOCK_STREAM 0 in

      connect s addr;

      let out_chan = out_channel_of_descr s in

      output_value out_chan (NetworkNewNode (nodetype, new_ip, new_port));
      flush out_chan;

      Printf.printf "Shared new node with %s:%d.\n%!"
        (string_of_inet_addr to_ip)
        to_port;

      Unix.close s )
  in

  NodeSet.iter share network

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

let greet_new_node miner_network nodetype server_ip server_port out_chan =
  output_value out_chan
    (NetworkMap (NodeSet.add (nodetype, server_ip, server_port) miner_network));
  flush out_chan
