open! Core
open! Async

(* Each subcommand is a [Command.t]. [Command.async_or_error] runs an Async main loop
   and turns an [Error] result into a non-zero exit. [%map_open.Command] (ppx_jane)
   declares flags; inside it, [flag "--name" ...] parses one command-line flag. *)

let cutoff_command =
  Command.async_or_error
    ~summary:"Print Kalshi's historical cutoff timestamps (checks network and TLS)"
    (let%map_open.Command () = return () in
     fun () ->
       let client = Kalshi.Client.create () in
       let%map.Deferred.Or_error c = Kalshi.Endpoints.cutoff client in
       print_s [%sexp (c : Kalshi.Endpoints.Cutoff.t)])
;;

let command =
  Command.group
    ~summary:"fade: is buying NO on Kalshi +EV after fees?"
    [ "fetch", Cmd_fetch.command
    ; "import-edgecheck", Cmd_import.command
    ; "freeze", Cmd_stubs.todo ~summary:"Choose rules in-sample and freeze them" ~phase:3
    ; ( "report"
      , Cmd_stubs.todo ~summary:"Print tables, write web/data/report.json" ~phase:3 )
    ; "serve", Cmd_stubs.todo ~summary:"Serve the dashboard on localhost" ~phase:4
    ; "synth", Cmd_synth.command
    ; "cutoff", cutoff_command
    ]
;;

let () = Command_unix.run command
