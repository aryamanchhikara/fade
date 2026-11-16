open! Core
open! Async

(* `fade serve`: a tiny static server for the dashboard, bound to localhost only.
   Routes are a fixed table (no path is ever joined from the request), so nothing
   outside these files can be read. *)

let content_type path =
  match Filename.split_extension path with
  | _, Some "html" -> "text/html; charset=utf-8"
  | _, Some "js" -> "application/javascript; charset=utf-8"
  | _, Some "css" -> "text/css; charset=utf-8"
  | _, Some "json" -> "application/json"
  | _ -> "application/octet-stream"
;;

let respond_file path =
  if Sys_unix.file_exists_exn path
  then
    Cohttp_async.Server.respond_with_file
      ~headers:
        (Cohttp.Header.of_list
           [ "content-type", content_type path; "cache-control", "no-store" ])
      path
  else
    Cohttp_async.Server.respond_string
      ~status:`Not_found
      (sprintf "%s not found. Did you run `make web` and `fade report`?\n" path)
;;

let command =
  Command.async
    ~summary:"Serve the dashboard (built JS + report.json) on localhost"
    ~readme:(fun () ->
      "Build the JavaScript first with `make web` (a release build of web/main.bc.js). \
       With --demo, the report is built in memory from the synthetic fixtures and \
       labelled as synthetic.")
    (let%map_open.Command source = Data_source.param
     and port = flag "--port" (optional_with_default 8080 int) ~doc:"PORT (default 8080)"
     and web_dir =
       flag "--web" (optional_with_default "web" string) ~doc:"DIR static files"
     and js =
       flag
         "--js"
         (optional_with_default "_build/default/web/main.bc.js" string)
         ~doc:"PATH built JavaScript bundle"
     and report =
       flag
         "--report"
         (optional string)
         ~doc:"PATH report.json (default web/data/report.json)"
     in
     fun () ->
       (* The demo report is computed once at startup, entirely in memory. *)
       let report_body =
         match source with
         | Data_source.Demo _ ->
           let r = Cmd_report.build_report source ~frozen_path:"rules/frozen.demo.json" in
           Some (Yojson.Safe.to_string (Fade_types.Report.yojson_of_t r))
         | Db _ -> None
       in
       let report_path = Option.value report ~default:(web_dir ^/ "data/report.json") in
       let handler ~body:_ _addr (req : Cohttp.Request.t) =
         match Uri.path (Cohttp.Request.uri req) with
         | "/" | "/index.html" -> respond_file (web_dir ^/ "index.html")
         | "/style.css" -> respond_file (web_dir ^/ "style.css")
         | "/main.bc.js" -> respond_file js
         | "/data/report.json" ->
           (match report_body with
            | Some body ->
              Cohttp_async.Server.respond_string
                ~headers:(Cohttp.Header.of_list [ "content-type", "application/json" ])
                body
            | None -> respond_file report_path)
         | _ -> Cohttp_async.Server.respond_string ~status:`Not_found "not found\n"
       in
       let%bind _server =
         Cohttp_async.Server.create
           ~on_handler_error:`Ignore
           (Tcp.Where_to_listen.bind_to Localhost (On_port port))
           handler
       in
       printf
         "fade dashboard%s: http://localhost:%d  (Ctrl-C to stop)\n%!"
         (if Data_source.is_demo source then " (SYNTHETIC demo data)" else "")
         port;
       Deferred.never ())
;;
