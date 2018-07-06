open Unix
open Printf

let establish_server server_fun sockaddr =

   let domain = domain_of_sockaddr sockaddr in
   let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
   Unix.bind sock sockaddr;
   Unix.listen sock 3;
   while true do
    let (s, caller) = Unix.accept sock in
        match Unix.fork() with
               0 -> let inchan = Unix.in_channel_of_descr s in
                    let outchan = Unix.out_channel_of_descr s in
                    begin
                    match caller with
                    | ADDR_UNIX s -> printf "%s\n" s
                    | ADDR_INET (a,b) -> printf "%s : %d\n" (string_of_inet_addr a) b
                    end;
                    server_fun inchan outchan ;
                    close_in inchan ;
                    close_out outchan ;
                    exit 0
             | id -> Unix.close s; ignore(Unix.waitpid [] id)

      done

let rec read_file f =
try
    let res = input_line f in
    if String.length res = 1
    then ()
    else
    let () = printf "%s\n" res in
    read_file f
with
| End_of_file -> ()

let html = "<html><body>test</body></html>
"
let rep =
"HTTP/1.1 200 OK
Content-Type: text/html
Connection: close

"^html


let () = establish_server (fun inn ou -> read_file inn; output_string ou rep; flush ou) (ADDR_INET(inet_addr_of_string "127.0.0.1",int_of_string Sys.argv.(1)))




(* printf 'GET / HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n' |
  nc www.example.com 80 *)
