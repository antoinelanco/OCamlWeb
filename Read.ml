let rec readFile f =
try
    let res = input_line f in
    if String.length res = 1
    then []
    else
    res :: (readFile f)
with
| End_of_file -> []

  
let readConfig =
  try
    let c = open_in "config.conf" in
    let r = Str.regexp "\\([a-z A-Z]+\\)\\([ ]*=[ ]*\\)\\(\\b\\)\\([ ]*\\)" in
    List.map
    (fun i ->
      let res = Str.global_replace r "\\1" i in
      let ls = Str.split_delim (Str.regexp " ") res in
      (List.hd ls,List.hd(List.tl ls)))
    (readFile c)


  with
  | _ -> failwith "Config file not found"


let getGet tab =
  let line = List.hd tab in
  let ls = Str.split_delim (Str.regexp " HTTP") line in
  Str.string_after (List.hd ls) 5
