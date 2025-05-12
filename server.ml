open Lwt.Infix
open Printf
open Cohttp
open Cohttp_lwt_unix

(* Helper function to load resources from a JSON file *)
let get_resources source =
  let source_file = source ^ ".json" in
  try
    Yojson.Basic.from_file source_file
  with
  | Sys_error msg ->
      `Assoc [ ("error", `String ("Failed to load resource: " ^ msg)) ]
  | Yojson.Json_error msg ->
      `Assoc [ ("error", `String ("Invalid JSON format: " ^ msg)) ]

(* Function to handle API routes *)
let handle_request uri =
  match uri with
  | "" | "/" ->
      `Assoc [ ("success", `String "Welcome to our test API using OCaml!") ]
  | "/api/users" | "/api/users/" ->
      get_resources "users"
  | "/api/books" | "/api/books/" ->
      get_resources "books"
  | _ ->
      `Assoc [ ("error", `String "Invalid endpoint!") ]

(* Main server logic *)
let () =
  let on_exn = function
    | Unix.Unix_error (error, func, arg) ->
        printf "Client connection error %s: %s(%S)\n%!" (Unix.error_message error) func arg
    | exn ->
        printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
  in

  let callback _conn req body =
    let uri = req |> Request.resource in
    let response_json = handle_request uri in
    let response = Yojson.Basic.pretty_to_string response_json in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
  
    (body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s" uri
        meth headers body )
    >>= fun _body -> Server.respond_string ~status:`OK ~body:response ()
  in

  let server = Server.make ~callback () in
  let port = 8080 in
  let mode = `TCP (`Port port) in
  printf "listening on http://localhost:%d\n%!" port;
  Server.create ~on_exn ~mode server |> Lwt_main.run
