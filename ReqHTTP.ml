open Read

let rep200 c =
"HTTP/1.1 200 OK
Location: /
Content-Type: text/html
Connection: close\n\n"^
(String.concat "\n" (readFile c))

let rep404 =
"HTTP/1.1 404 Not Found
Location: /
Content-Type: text/html
Connection: close\n
<!DOCTYPE html>
<html>
  <head>
    <title>404 Not Found</title>
  </head>
  <body>
    Error 404 File Not Found
  </body>
</html>"
