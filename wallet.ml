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

let blockchain_headers = ref [ genesis ]

let my_transactions = ref []

(*
  Function: print_new_network
  Prints the miner network
*)
let print_new_network network =
  print_endline "New network:";
  print_NodeSet network

(*
  Function: send_transaction
  Ask user to input transaction destination and amount
  and send the transaction to the miner at miner_addr.

  Arguments:
    my_name, the name of the current wallet
    miner_addr, listening address of the remote miner
*)
let send_transaction my_name miner_addr =
  print_endline "To who ? : ";
  let destination = read_line () in
  print_endline "What amount ? : ";
  let amount = read_float () in

  let transaction = make_transaction my_name destination amount in
  Printf.printf "Transaction hash = %s\n" (hash transaction);
  my_transactions := transaction :: !my_transactions;

  let s = socket PF_INET SOCK_STREAM 0 in
  connect s miner_addr;

  let out_chan = out_channel_of_descr s in
  output_value out_chan (Transaction transaction);
  flush out_chan;
  Unix.close s

(*
  Function show_blockchain_header
  Ask the remote miner at miner_addr for the block header chain.
*)
let show_blockchain_header miner_addr =
  let s = socket PF_INET SOCK_STREAM 0 in
  connect s miner_addr;

  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in

  output_value out_chan ShowBlockchainHeader;
  flush out_chan;

  match input_value in_chan with
  | BlockchainHeader b ->
      Unix.close s;
      b
  | _ -> raise (NotUnderstood "Expected BlockchainHeader.")

(*
  Function: find_block_by_id_update_on_error
  Look for a block in the chain by its id. If none is found, update
  the blockchain by asking to remote miner at miner_addr.

  Arguments:
    block_id, the id of the block to find
    miner_addr, the remote miner listening address
*)
let find_block_by_id_update_on_error block_id miner_addr =
  try (
    find_block_by_id !blockchain_headers block_id
  )
  with Not_found -> (
    blockchain_headers := show_blockchain_header miner_addr;
    find_block_by_id !blockchain_headers block_id
  )

(*
  Function: confirm_transaction
  Ask the user for a transaction hash to confirm and ask the remote
  miner at miner_addr for a merkle proof. Then print the confirmation 
  or the error.

  Arguments:
    miner_addr, the remote miner listening address
*)
let confirm_transaction miner_addr =
  print_string "> Transaction hash = ";
  let thash = read_line () in

  (try 
    let _ = find_transaction_by_hash !my_transactions thash in ()
  with
    Not_found -> print_endline "Warning : No transaction was found with this ID.");

  let s = socket PF_INET SOCK_STREAM 0 in
  connect s miner_addr;

  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in

  output_value out_chan (Confirmation thash);
  flush out_chan;

  (match input_value in_chan with
  | TransactionExist (proof, block_id) -> (
      try
        let block = find_block_by_id_update_on_error block_id miner_addr in
        let is_in_tree = Merkle.authenticate thash proof block.merkle_root in 
        if is_in_tree then
          print_endline ( "Transaction confirmed in block of ID=" ^ (string_of_int block.id) )
        else
          print_endline "Error : The proof check wasn't successful."
      with
        Not_found -> print_endline "Error : The block of proof could not be found."
  )
  | TransactionNotExist ->
      print_endline "The transaction has not been found"
  | TransactionWaiting ->
      print_endline "The transaction is waiting to be added"
  | _ -> raise (NotUnderstood "Answer not understood."));

  Unix.close s

(*
  Function: main
  Infinite loop of reading user input in the terminal.
*)
let main () =
  (* command argument check *)
  if !server_port = 0 || !name = "" then (
    Arg.usage speclist usage_msg;
    exit 0 );

  let miner_addr = ADDR_INET (!server_ip, !server_port) in

  network := show_peers miner_addr;
  Printf.printf "Réponse reçue de la part du miner distant\nNetwork: %!";
  print_NodeSet !network;
  
  blockchain_headers := show_blockchain_header miner_addr;
  print_string "Blockchain : \n";
  print_endline (string_of_blockchain_headers !blockchain_headers);

  let usage = "Usage : help | Show peers (1) | Send transaction (2) | Refresh blockchain (3) | Confirm transaction (4)" in
  print_endline usage;

  try
    while true do
      print_string "> ";
      match read_line () with
        | "help" -> print_endline usage
        | "1" -> print_NodeSet !network
        | "2" -> send_transaction !name miner_addr
        | "3" -> (blockchain_headers := show_blockchain_header miner_addr;
          print_string "Blockchain : \n";
          print_endline (string_of_blockchain_headers !blockchain_headers))
        | "4" -> confirm_transaction miner_addr
        | _ -> print_endline "Command not understood."
    done
  with NotUnderstood e -> print_endline e

let () = main ()
