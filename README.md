# simple_ocaml_rest_api_server

A simple OCaml REST API using `cohttp` that reads and returns json files of list of users and books

## How to setup the project

- clone the repository
- Run `opam switch create . ocaml-base-compiler.5.4.0` to create a local switch
- Run `eval $(opam env)` to activate switch
- Run `opam install . --deps-only` to install all project dependencies
- Run `dune exec ./server.exe` to start the application
- Open `localhost:8080` in the browser to view the project frontend
- Open `http://localhost:8080/api/users` in the browser to view users json document
- Open `http://localhost:8080/api/users/<id>` in the browser to view a user based on id from json document
- Open `http://localhost:8080/api/books` in the browser to view books json document
- Open `http://localhost:8080/api/books/<id>` in the browser to view a book based on id from json document
