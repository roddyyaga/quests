open Lwt

module Request = struct
  type t = {
    url: string;
    headers: Cohttp.Header.t; [@printer Cohttp.Header.pp_hum]
  }
  [@@deriving show]
end

module Response = struct
  (* OCaml strings are just bytes, no encoding, so no text *)

  type t = {
    content: string;
        [@printer
          fun fmt s -> fprintf fmt {|"%s"|} (Utils.escaped_literal_newlines s)]
    status_code: int;
    headers: Cohttp.Header.t; [@printer Cohttp.Header.pp_hum]
    request: Request.t; (* TODO - custom pp as list of tuples *)
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
  let { Requests.meth; body; request_headers; uri } =
    Requests.make_request_data meth ?data ?params ?headers ?auth url
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
      (* Don't change method to GET on 301 (even though it's somewhat permitted) *)
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
        headers = response_headers;
        request = { Request.url = Uri.to_string uri; headers = request_headers };
      }

let get ?data ?headers ?auth = request `GET ?data ?headers ?auth

let post ?data ?headers ?auth = request `POST ?data ?headers ?auth

let put ?data ?headers ?auth = request `PUT ?data ?headers ?auth

let delete ?data ?headers ?auth = request `DELETE ?data ?headers ?auth
