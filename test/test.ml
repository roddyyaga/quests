open Lwt

let test_get _ () =
  Quests.get "https://postman-echo.com/get" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_post _ () =
  Quests.post "https://postman-echo.com/post" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_put _ () =
  Quests.put "https://postman-echo.com/put" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_delete _ () =
  Quests.delete "https://postman-echo.com/delete" >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code

let test_basic_auth _ () =
  Quests.(
    get "https://postman-echo.com/basic-auth"
      ~auth:(Basic ("postman", "password")))
  >>= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 200 r.Quests.Response.status_code
  |> Lwt.return
  >>= fun () ->
  Quests.(
    get "https://postman-echo.com/basic-auth"
      ~auth:(Basic ("postman", "wrong_password")))
  >|= fun r ->
  print_endline @@ Quests.Response.show r;
  Alcotest.(check int) "status code" 401 r.Quests.Response.status_code

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "Quests"
       [
         ( "methods",
           [
             test_case "Get" `Quick test_get;
             test_case "Post" `Quick test_post;
             test_case "Put" `Quick test_put;
             test_case "Delete" `Quick test_delete;
           ] );
         ("authentication", [ test_case "Basic" `Quick test_basic_auth ]);
       ]
