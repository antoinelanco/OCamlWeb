open Unix
open Printf
open Read
open Printer
open ReqHTTP

let ipAddr = inet_addr_of_string (List.assoc "ip" readConfig)
let port = int_of_string (List.assoc "port" readConfig)
let rootDir = List.assoc "root" readConfig

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
                    | ADDR_UNIX s ->
                      let () = printf "%s\n" s in
                      server_fun inchan outchan s
                    | ADDR_INET (a,b) ->
                      let () = printf "%s : %d\n\n" (string_of_inet_addr a) b in
                      server_fun inchan outchan (string_of_inet_addr a)
                    end;
                    let () = close_in inchan in
                    exit 0
             | id -> Unix.close s; ignore(Unix.waitpid [] id)

      done


let inout input output ip =
  let tab = readFile input in
  let file = getGet tab in
  let () = printReq tab in
  begin
  try
    let c = open_in (rootDir^file) in
    output_string output (rep200 c);
  with
  | _ -> output_string output rep404
  end;
  flush output

let () = establish_server inout
(ADDR_INET(ipAddr,port))
