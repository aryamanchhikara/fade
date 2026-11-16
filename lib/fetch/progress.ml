open! Core

(* A one-line progress bar on stderr, redrawn at most twice a second. *)
type t =
  { label : string
  ; total : int option
  ; started : Time_ns.t
  ; mutable done_ : int
  ; mutable last_draw : Time_ns.t
  ; mutable note : string
  }

let create ?total label =
  { label
  ; total
  ; started = Time_ns.now ()
  ; done_ = 0
  ; last_draw = Time_ns.epoch
  ; note = ""
  }
;;

let human_duration secs =
  let secs = Float.iround_nearest_exn secs in
  if secs < 60
  then sprintf "%ds" secs
  else if secs < 3600
  then sprintf "%dm%02ds" (secs / 60) (secs mod 60)
  else sprintf "%dh%02dm" (secs / 3600) (secs mod 3600 / 60)
;;

let line t =
  let elapsed = Time_ns.Span.to_sec (Time_ns.diff (Time_ns.now ()) t.started) in
  let rate = if Float.( > ) elapsed 0. then Float.of_int t.done_ /. elapsed else 0. in
  let counts, eta =
    match t.total with
    | Some total when total > 0 ->
      let width = 24 in
      let filled = t.done_ * width / total in
      let bar = String.make filled '#' ^ String.make (width - filled) '.' in
      let eta =
        if Float.( > ) rate 0.
        then " ETA " ^ human_duration (Float.of_int (total - t.done_) /. rate)
        else ""
      in
      ( sprintf
          "[%s] %d/%d %.1f%%"
          bar
          t.done_
          total
          (100. *. Float.of_int t.done_ /. Float.of_int total)
      , eta )
    | _ -> sprintf "%d" t.done_, ""
  in
  sprintf "%-18s %s %.1f/s%s %s" t.label counts rate eta t.note
;;

let draw ?(force = false) t =
  let now = Time_ns.now () in
  if force || Time_ns.Span.( > ) (Time_ns.diff now t.last_draw) (Time_ns.Span.of_sec 0.5)
  then (
    t.last_draw <- now;
    (* \r returns to the start of the line; \027[K clears the rest of it. *)
    eprintf "\r%s\027[K%!" (line t))
;;

let advance ?(by = 1) ?note t =
  t.done_ <- t.done_ + by;
  Option.iter note ~f:(fun n -> t.note <- n);
  draw t
;;

let finish t =
  draw ~force:true t;
  eprintf "\n%!"
;;
