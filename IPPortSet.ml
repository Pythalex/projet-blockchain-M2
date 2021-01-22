module IPPortSet = Set.Make(
    struct
        let compare (ip1, port1) (ip2, port2) =
             Stdlib.compare port1 port2
        type t = Unix.inet_addr * int
    end
)

let printPair (ip, port) =
    Printf.printf "\t(%s, %d)\n" (Unix.string_of_inet_addr ip) port

let printSet set =

    let printIter elt =
        printPair elt;
    in

    print_string "{\n";
    IPPortSet.iter printIter set;
    print_string "}\n";
    flush stdout