open Lwt.Infix
open Printf
open Cohttp
open Cohttp_lwt_unix

let get_resources source =
  let source_file = source ^ ".json" in
  let resources = Yojson.Basic.from_file source_file in
  resources

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
          let response =
            `Assoc
              [ ("success", `String "Welcome to our test API using OCaml!") ]
          in
          Yojson.Basic.pretty_to_string response
      | "/api/users" -> Yojson.Basic.pretty_to_string (get_resources "users")
      | "/api/books" -> Yojson.Basic.pretty_to_string (get_resources "books")
      | _ ->
          let response = `Assoc [ ("error", `String "There was an error!") ] in
          Yojson.Basic.pretty_to_string response
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
