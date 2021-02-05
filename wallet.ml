open Unix
open Common

(* network variables *)
let name = ref ""

let server_ip = ref inet_addr_loopback

let server_port = ref 0

let set_connect_to_ip ip = server_ip := inet_addr_of_string ip

let speclist =
  [
    ("-p", Arg.Set_int server_port, "Remote miner port");
    ("-i", Arg.String set_connect_to_ip, "Remote miner ip");
    ("-n", Arg.Set_string name, "Wallet id");
  ]

let usage_msg = "Super wallet"

;;
Arg.parse speclist print_endline usage_msg

(* application variables *)
let mynodetype = Wallet

let network = ref NodeSet.empty

let print_new_network network =
  print_endline "New network:";
  print_NodeSet network
 
let main () =
  (* command argument check *)
  if !server_port = 0 then (
    Arg.usage speclist usage_msg;
    exit 0 );

  let miner_addr = ADDR_INET (!server_ip, !server_port) in

  network := show_peers miner_addr;
  Printf.printf "Réponse reçue de la part du miner distant\n%!";
  print_NodeSet !network;


  Printf.printf "Usage : Show peers (1)";
  while true do
    match read_line () with
      | "1" -> print_NodeSet !network
      | _ -> print_endline "Command not understood."
  done

let () = main ()
