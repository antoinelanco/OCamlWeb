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

let rec read_file f =
try
    let res = input_line f in
    if String.length res = 1
    then []
    else
    res :: (read_file f)
with
| End_of_file -> []


let printReq tab = printf "%s\n------------------\n" (String.concat "\n" tab)

let getGet tab =
  let line = List.hd tab in
  let ls = Str.split_delim (Str.regexp " HTTP") line in
  Str.string_after (List.hd ls) 5

let repOK c =
"HTTP/1.1 200 OK
Location: /
Content-Type: text/html
Connection: close\n\n"^
(String.concat "\n" (read_file c))

let repNOK =
"HTTP/1.1 404 Not Found
Location: /
Content-Type: text/html
Connection: close\n
<!DOCTYPE html><html><head><title>Not Found</title></head><body> File Not Found </body></html>"



let inout input output ip =
  let tab = read_file input in
  let file = getGet tab in
  let () = printReq tab in
  begin
  try
    let c = open_in file in
    output_string output (repOK c)
  with
  | _ -> output_string output repNOK
  end;
  flush output

let () = establish_server inout
(ADDR_INET(inet_addr_of_string "129.175.19.199",int_of_string Sys.argv.(1)))
