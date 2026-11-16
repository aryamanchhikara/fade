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

let () = Bonsai_web.Start.start counter
