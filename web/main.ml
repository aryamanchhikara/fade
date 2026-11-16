(* Phase 0 smoke test: a Bonsai counter. Replaced by the real dashboard in Phase 4. *)
open! Core
open Bonsai_web.Cont
open Bonsai.Let_syntax

(* A variant type: a value is exactly one of these constructors. The compiler checks
   that every [match] handles all of them. [@@deriving sexp_of] generates a printer so
   Bonsai can log actions. *)
module Action = struct
  type t =
    | Incr
    | Decr
  [@@deriving sexp_of]
end

let counter (graph : Bonsai.graph) =
  (* [state_machine0] instead of [state]: the button sends an action, and
     [apply_action] runs it against the *current* model. With plain [state] and
     [set (count + 1)], three fast clicks before a re-render would all write 1. *)
  let count, inject =
    Bonsai.state_machine0
      ~sexp_of_action:Action.sexp_of_t
      ~default_model:0
      ~apply_action:(fun _ctx model -> function
         | Action.Incr -> model + 1
         | Decr -> model - 1)
      graph
  in
  (* [let%arr] (from ppx_bonsai) reads the current values of Bonsai values and
     recomputes the view only when one of them changes. *)
  let%arr count = count
  and inject = inject in
  let button label action =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> inject action) ]
      [ Vdom.Node.text label ]
  in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "fade: Bonsai is alive" ]
    ; button "-1" Decr
    ; Vdom.Node.span
        ~attrs:
          [ Vdom.Attr.id "count"
          ; Vdom.Attr.style (Css_gen.margin ~left:(`Px 12) ~right:(`Px 12) ())
          ]
        [ Vdom.Node.textf "%d" count ]
    ; button "+1" Incr
    ]
;;

(* Phase 1 check that lib/types compiles to JavaScript: decode a [Market.t] from JSON
   with the very same [t_of_yojson] the native CLI uses. *)
let shared_types_check =
  let json =
    {|{"ticker":"SYNX-1-M0","event_ticker":"SYNX-1","series_ticker":"SYNX","category":"Demo","open_time":1747353600,"close_time":1747440000,"settle_time":1747440300,"result":["No"],"volume":1234.5,"can_close_early":false,"event_kind":["Bracket"]}|}
  in
  let m = Fade_types.Market.t_of_yojson (Yojson.Safe.from_string json) in
  Vdom.Node.p
    ~attrs:[ Vdom.Attr.id "shared" ]
    [ Vdom.Node.textf
        "Decoded in the browser by Fade_types: %s resolved %s, %s"
        m.ticker
        (Fade_types.Side.to_string m.result)
        (Fade_types.Event_kind.to_string m.event_kind)
    ]
;;

let app graph =
  let%arr counter = counter graph in
  Vdom.Node.div [ counter; shared_types_check ]
;;

let () = Bonsai_web.Start.start app
