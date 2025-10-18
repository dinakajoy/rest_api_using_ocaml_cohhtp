open Lwt.Infix
open Printf
open Cohttp
open Cohttp_lwt_unix

(* Function to get a specific resource by ID *)
let get_resource_by_id source id =
  let source_file = source ^ ".json" in
  try
    let json = Yojson.Basic.from_file source_file in
    match json with
    | `List items ->
        let result =
          List.find_opt
            (fun item ->
              match item with
              | `Assoc props ->
                  (match List.assoc_opt "id" props with
                  | Some (`String item_id) -> item_id = id
                  | Some (`Int item_id) -> string_of_int item_id = id
                  | _ -> false)
              | _ -> false) items
        in
        (match result with
        | Some item -> item
        | None ->
            `Assoc [ ("error", `String ("Resource with id " ^ id ^ " not found")) ])
    | _ ->
        `Assoc [ ("error", `String "Invalid JSON structure") ]
  with
  | Sys_error msg ->
      `Assoc [ ("error", `String ("Failed to load resource: " ^ msg)) ]
  | Yojson.Json_error msg ->
      `Assoc [ ("error", `String ("Invalid JSON format: " ^ msg)) ]   

(* Function to get all resources *)
let get_resources source =
  let source_file = source ^ ".json" in
  try
    Yojson.Basic.from_file source_file
  with
  | Sys_error msg ->
      `Assoc [ ("error", `String ("Failed to load resource: " ^ msg)) ]
  | Yojson.Json_error msg ->
      `Assoc [ ("error", `String ("Invalid JSON format: " ^ msg)) ]

let split_path uri =
  Uri.path uri
  |> String.split_on_char '/'
  |> List.filter (fun s -> s <> "")

(* Function to handle API routes *)
let handle_request uri =
  let segments = split_path uri in
  match segments with
  | [] ->
    `Assoc [ ("success", `String "Welcome to our test API using OCaml!") ]
  | [ "api"; "users" ] ->
    get_resources "users"
  | [ "api"; "users"; id ] ->
    get_resource_by_id "users" id
  | [ "api"; "books" ] ->
    get_resources "books"
  | [ "api"; "books"; id ] ->
    get_resource_by_id "books" id
  | _ ->
    `Assoc [ ("error", `String "Invalid endpoint!") ]

let () =
  let on_exn = function
    | Unix.Unix_error (error, func, arg) ->
        printf "Client connection error %s: %s(%S)\n%!" (Unix.error_message error) func arg
    | exn ->
        printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
  in

  let callback _conn req body =
    let uri = Request.uri req in
    let response_json = handle_request uri in
    let response = Yojson.Basic.pretty_to_string response_json in
    let meth = req |> Request.meth |> Code.string_of_method in
    let headers = req |> Request.headers |> Header.to_string in
  
    (body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s" (Uri.to_string uri)
        meth headers body )
    >>= fun _body -> Server.respond_string ~status:`OK ~body:response ()
  in

  let server = Server.make ~callback () in
  let port = 8080 in
  let mode = `TCP (`Port port) in
  printf "listening on http://localhost:%d\n%!" port;
  Server.create ~on_exn ~mode server |> Lwt_main.run
