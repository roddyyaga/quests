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
