open Lwt

(* [String.escaped] but not escaping newlines *)
let escaped_literal_newlines s =
  let lines = String.split_on_char '\n' s in
  String.concat "\n" (List.map String.escaped lines)

module Response = struct
  (* OCaml strings are just bytes, no encoding, so no text *)

  type t = {
    content: string;
        [@printer
          fun fmt s -> fprintf fmt {|"%s"|} (escaped_literal_newlines s)]
    status_code: int;
    headers: (string * string) list;
  }
  [@@deriving show]

  let content t = t.content

  let status_code t = t.status_code

  let headers t = t.headers

  let ok { status_code; _ } = 200 <= status_code && status_code < 400

  let json { content; _ } = Yojson.Safe.from_string content

  let result_for_status response =
    if ok response then Ok response else Error response
end

type payload =
  | Json of Yojson.t
  | Form of (string * string) list
  | Raw of string

type authentication = Basic of string * string | Bearer of string

let data_to_body data =
  data
  |> List.map (fun (key, value) -> (key, [ value ]))
  |> Uri.encoded_of_query |> Cohttp_lwt.Body.of_string

let json_to_body json = json |> Yojson.to_string |> Cohttp_lwt.Body.of_string

let resolve_location_uri ~location_uri ~reference_uri =
  match Uri.host location_uri with
  | Some _ ->
      (* Absolute *)
      location_uri
  | None ->
      (* Relative *)
      (* TODO - check complies with RFC 3986 *)
      Uri.(with_path reference_uri (path location_uri))

let rec request meth ?data ?params ?headers ?auth ?(follow_redirects = true) url
    =
  let request_headers = Cohttp.Header.init () in
  let request_headers =
    Cohttp.Header.add request_headers "accept-encoding" "gzip"
  in
  let body, request_headers =
    let body, new_headers_list =
      match data with
      | Some (Form data) ->
          ( Some (data_to_body data),
            [ ("Content-Type", "application/x-www-form-urlencoded") ] )
      | Some (Json json) ->
          (Some (json_to_body json), [ ("Content-Type", "application/json") ])
      | Some (Raw s) -> (Some (Cohttp_lwt.Body.of_string s), [])
      | None -> (None, [])
    in
    (body, Cohttp.Header.add_list request_headers new_headers_list)
  in
  let request_headers =
    match auth with
    | Some (Basic (username, password)) ->
        Cohttp.Header.add_authorization request_headers
          (`Basic (username, password))
    | Some (Bearer s) ->
        Cohttp.Header.add_authorization request_headers (`Other ("Bearer " ^ s))
    | None -> request_headers
  in
  let request_headers =
    match headers with
    | Some specified_headers ->
        Cohttp.Header.add_list request_headers specified_headers
    | None -> request_headers
  in
  let uri =
    match params with
    | Some params -> Uri.(add_query_params' (Uri.of_string url) params)
    | None -> Uri.of_string url
  in
  Cohttp_lwt_unix.Client.call meth ?body ~headers:request_headers uri
  >>= fun (response, body) ->
  let status_code =
    response |> Cohttp.Response.status |> Cohttp.Code.code_of_status
  in
  let response_headers = response |> Cohttp.Response.headers in
  match
    (status_code, follow_redirects, Cohttp.Header.get_location response_headers)
  with
  | 301, true, Some location_uri
  | 302, true, Some location_uri
  | 307, true, Some location_uri
  | 308, true, Some location_uri ->
      let location_uri =
        resolve_location_uri ~location_uri ~reference_uri:uri
      in
      Printf.printf "Location: %s\n" (Uri.to_string location_uri);
      request meth ?data ?params ?headers ?auth ~follow_redirects
        (Uri.to_string location_uri)
  | 303, true, Some location_uri ->
      (* Change method to GET and lose body *)
      let location_uri =
        resolve_location_uri ~location_uri ~reference_uri:uri
      in
      Printf.printf "Location: %s\n" (Uri.to_string location_uri);
      request `GET ?data:None ?params ?headers ?auth ~follow_redirects
        (Uri.to_string location_uri)
  | _ ->
      body |> Cohttp_lwt.Body.to_string >|= fun content ->
      let content =
        match Cohttp.Header.get response_headers "content-encoding" with
        | Some "gzip" -> Ezgzip.decompress content |> Result.get_ok
        | Some _ | None -> content
      in
      {
        Response.content;
        status_code;
        headers = response_headers |> Cohttp.Header.to_list;
      }

let get ?data ?headers ?auth = request `GET ?data ?headers ?auth

let post ?data ?headers ?auth = request `POST ?data ?headers ?auth

let put ?data ?headers ?auth = request `PUT ?data ?headers ?auth

let delete ?data ?headers ?auth = request `DELETE ?data ?headers ?auth
