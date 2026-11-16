open! Core

(* Placeholders so the CLI shape exists from Phase 1; each is replaced in its phase. *)
let todo ~summary ~phase =
  Command.basic
    ~summary
    (Command.Param.return (fun () ->
       eprintf "not implemented yet (Phase %d)\n" phase;
       exit 2))
;;
