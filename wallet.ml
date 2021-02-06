open Unix
open Common

(* network variables *)
let name = ref ""

let server_ip = ref inet_addr_loopback

let server_port = ref 0

let set_connect_to_ip ip = server_ip := inet_addr_of_string ip

let speclist =
  [
    ("-p", Arg.Set_int server_port, "Remote miner port [REQUIRED]");
    ("-i", Arg.String set_connect_to_ip, "Remote miner ip (default loopback)");
    ("-n", Arg.Set_string name, "Wallet id [REQUIRED]");
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

let send_transaction my_name miner_addr =
  print_endline "To who ? : ";
  let destination = read_line () in
  print_endline "What amount ? : ";
  let amount = read_float () in

  let transaction = {source = my_name; destination = destination; amount = amount} in
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s miner_addr;

  let out_chan = out_channel_of_descr s in
  output_value out_chan (Transaction transaction);
  flush out_chan;
  Unix.close s


let main () =
  (* command argument check *)
  if !server_port = 0 || !name = "" then (
    Arg.usage speclist usage_msg;
    exit 0 );

  let miner_addr = ADDR_INET (!server_ip, !server_port) in

  network := show_peers miner_addr;
  Printf.printf "Réponse reçue de la part du miner distant\n%!";
  print_NodeSet !network;

  let usage = "Usage : help | Show peers (1) | Send transaction (2)" in
  print_endline usage;
  while true do
    print_string "> ";
    match read_line () with
      | "help" -> print_endline usage
      | "1" -> print_NodeSet !network
      | "2" -> send_transaction !name miner_addr
      | _ -> print_endline "Command not understood."
  done

let () = main ()
