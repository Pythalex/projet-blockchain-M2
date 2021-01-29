open Unix 

module Inte = struct 
  type t = Unix.inet_addr * int
  let compare (ip1, port1) (ip2, port2) =
    Stdlib.compare port1 port2

 end

 module InetSet = Set.Make(Inte)
 let printPair (ip, port) =
  Printf.printf "\t(%s, %d)\n%!"
    (string_of_inet_addr ip)
    port
 let print_set s =
  InetSet.iter printPair s;
  flush Stdlib.stdout

