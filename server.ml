open Lwt.Infix
open Printf
open Cohttp
open Cohttp_lwt_unix

type user = { id : int; name : string; location : string }
type book = { id : int; name : string; user : int }

let users_of_json = function
  | `O
      [
        ("id", `Int id); ("name", `String name); ("location", `String location);
      ] ->
      { id; name; location }
  | _ ->
      Ezjsonm.to_string
        (`Assoc [ ("success", `String "Welcome to our test API using OCaml!") ])

let books_of_json = function
  | `O [ ("id", `Int id); ("name", `String name); ("user", `Int user) ] ->
      { id; name; user }
  | _ ->
      Ezjsonm.to_string
        (`Assoc [ ("success", `String "Welcome to our test API using OCaml!") ])

let get_resorces source =
  let source_file = open_in (source ^ ".json") in
  let resorces = Ezjsonm.value_from_channel source_file in
  close_in source_file;
  resorces

let users_handler =
  let users = get_resorces "users" in
  match users with
  | `A users -> (
      try Ok (List.map (fun p -> Ezjsonm.to_string (users_of_json p)) users)
      with _ ->
        Ezjsonm.to_string
          (`Assoc
            [ ("success", `String "Welcome to our test API using OCaml!") ]))
  | _ -> Ezjsonm.to_string []

let books_handler =
  let users = get_resorces "books" in
  match users with
  | `A users -> (
      try Ok (List.map (fun p -> Ezjsonm.to_string (books_of_json p)) users)
      with _ ->
        Ezjsonm.to_string
          (`Assoc
            [ ("success", `String "Welcome to our test API using OCaml!") ]))
  | _ -> Ezjsonm.to_string []

let () =
  let on_exn = function
    | Unix.Unix_error (error, func, arg) ->
        printf "Client connection error %s: %s(%S)" (Unix.error_message error)
          func arg
    | exn -> printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
  in
  let callback _conn req body =
    let uri = req |> Request.resource in
    let response =
      match uri with
      | "" | "/" ->
          Ezjsonm.to_string
            (`Assoc
              [ ("success", `String "Welcome to our test API using OCaml!") ])
      | "/api/users" -> users_handler
      | "/api/books" -> books_handler
      | _ ->
          Ezjsonm.to_string
            (`Assoc [ ("error", `String "There was an error!") ])
    in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
    ( body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s" uri
        meth headers body )
    >>= fun _body -> Server.respond_string ~status:`OK ~body:response ()
  in
  let server = Server.make ~callback () in
  let port = 8080 in
  let mode = `TCP (`Port port) in
  printf "listening on http://localhost:%d\n%!" port;
  Server.create ~on_exn ~mode server |> Lwt_main.run
