![Build](https://github.com/roddyyaga/quests/workflows/Build/badge.svg)

# Quests

Quests is an HTTP library built on top of [Cohttp](https://github.com/mirage/ocaml-cohttp). The API is closely inspired by [Python's Requests](https://github.com/psf/requests/).

## Installation

## Usage
```ocaml
open Lwt

let get () =
  Quests.get "http://httpbin.org/get"
    ~params:[ ("key1", "value1"); ("key2", "value2") ]
  >|= Quests.Response.show >|= print_endline

let post_form () =
  Quests.post "http://httpbin.org/post" ~data:(Form [ ("key", "value") ])
  >|= Quests.Response.show >|= print_endline

let post_json () =
  Quests.post "http://httpbin.org/post" ~data:(Json [%yojson { key = "value" }])
  >|= Quests.Response.show >|= print_endline

let gzip_response () =
  Quests.get "http://httpbin.org/gzip"
  >|= Quests.Response.show >|= print_endline

let following_redirects () =
  Quests.get "http://httpbin.org/redirect/1"
  >|= Quests.Response.show >|= print_endline

let () =
  Lwt_main.run
    (Lwt_list.iter_s
       (fun f -> f ())
       [ get; post_form; post_json; gzip_response; following_redirects ])
```

See `bin/examples.ml`.

## Features
- Automatically handles gzipped responses
- Can follow redirects
- Basic or bearer authentication

## TODO
- [ ] - sessions/connection pooling
- [ ] - cookies
- [ ] - digest authentication
- [ ] - other compression schemes

Contributions welcome!
