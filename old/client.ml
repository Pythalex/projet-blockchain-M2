open Unix

(* network variables *)
let ip = inet_addr_loopback
let port = 8000

let addr = ADDR_INET(ip, port) 

(* création de la socket IPv4, TCP *)
let s = socket PF_INET SOCK_STREAM 0

let () =

  connect s addr;
  let in_chan = in_channel_of_descr s in
  let out_chan = out_channel_of_descr s in

  (*let in_chan, out_chan = open_connection addr*)
  
  while true do
    let m =  read_line() in
    if m = "fin" then (close s; exit 0);
    output_string out_chan (m^"\n");
    flush out_chan;
    let r = input_line in_chan in
    Format.printf "%s@." r
  done