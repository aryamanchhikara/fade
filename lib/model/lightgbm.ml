open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Gradient-boosted trees via the LightGBM command-line tool (`brew install lightgbm`).
   We write the rows to CSV (label first, no header), run `lightgbm task=train`, and
   read predictions back from `task=predict`. Boosting adds small trees one at a time,
   each fitted to the current errors; early stopping on validation rows picks how many.

   The model is constrained to be non-decreasing in the NO price: a higher ask never
   lowers P(NO). Anything else it learns has to be on top of that. *)

module Params = struct
  type t =
    { num_leaves : int
    ; min_data_in_leaf : int
    ; learning_rate : float
    ; lambda_l2 : float
    ; feature_fraction : float
    }
  [@@deriving sexp, yojson, compare, equal]

  let to_string t =
    sprintf
      "leaves %d, min leaf %d, lr %g"
      t.num_leaves
      t.min_data_in_leaf
      t.learning_rate
  ;;
end

let binary = "lightgbm"

let available () =
  match Sys_unix.command (sprintf "command -v %s > /dev/null 2>&1" binary) with
  | 0 -> true
  | _ -> false
;;

let write_csv path ~(x : float array array) ~(y : bool array option) =
  Out_channel.with_file path ~f:(fun oc ->
    Array.iteri x ~f:(fun i row ->
      let label =
        match y with
        | Some y -> if y.(i) then "1" else "0"
        | None -> "0"
      in
      Out_channel.output_string oc label;
      Array.iter row ~f:(fun v ->
        Out_channel.output_char oc ',';
        Out_channel.output_string oc (sprintf "%.10g" v));
      Out_channel.output_char oc '\n'))
;;

let run args ~log =
  let cmd =
    String.concat ~sep:" " (binary :: List.map args ~f:Filename.quote)
    ^ " > "
    ^ Filename.quote log
    ^ " 2>&1"
  in
  match Sys_unix.command cmd with
  | 0 -> ()
  | code ->
    raise_s
      [%message
        "lightgbm failed"
          (code : int)
          ~log_tail:(String.suffix (In_channel.read_all log) 2000 : string)]
;;

(* "Early stopping at iteration 104, the best iteration round is 74" *)
let best_iteration log =
  In_channel.read_lines log
  |> List.find_map ~f:(fun line ->
    match String.substr_index line ~pattern:"the best iteration round is " with
    | Some i ->
      Int.of_string_opt
        (String.strip
           (String.drop_prefix line (i + String.length "the best iteration round is ")))
    | None -> None)
;;

let trees_in model =
  In_channel.read_lines model |> List.count ~f:(String.is_prefix ~prefix:"Tree=")
;;

(* Trains on [x]/[y]. With [valid], early-stops on it (patience 50) and returns the best
   iteration; otherwise runs exactly [iterations]. Returns the model file and how many
   trees it has. *)
let train ~dir ~(params : Params.t) ~monotone ~seed ~x ~y ?valid ?(iterations = 1000) () =
  Core_unix.mkdir_p dir;
  let train_csv = dir ^/ "train.csv"
  and valid_csv = dir ^/ "valid.csv"
  and model = dir ^/ "model.txt"
  and log = dir ^/ "train.log" in
  write_csv train_csv ~x ~y:(Some y);
  let valid_args =
    match valid with
    | None -> []
    | Some (vx, vy) ->
      write_csv valid_csv ~x:vx ~y:(Some vy);
      [ "valid=" ^ valid_csv; "early_stopping_round=50" ]
  in
  run
    ([ "task=train"
     ; "data=" ^ train_csv
     ; "objective=binary"
     ; "metric=binary_logloss"
     ; sprintf "num_iterations=%d" iterations
     ; sprintf "learning_rate=%g" params.learning_rate
     ; sprintf "num_leaves=%d" params.num_leaves
     ; sprintf "min_data_in_leaf=%d" params.min_data_in_leaf
     ; sprintf "lambda_l2=%g" params.lambda_l2
     ; sprintf "feature_fraction=%g" params.feature_fraction
     ; "monotone_constraints="
       ^ String.concat ~sep:"," (List.map (Array.to_list monotone) ~f:Int.to_string)
     ; "monotone_constraints_method=advanced"
     ; "output_model=" ^ model
     ; sprintf "seed=%d" seed
     ; "deterministic=true"
     ; "force_row_wise=true"
     ; "num_threads=4"
     ; "verbosity=1"
     ]
     @ valid_args)
    ~log;
  let trees = Option.value (best_iteration log) ~default:(trees_in model) in
  model, trees
;;

let predict ~dir ~model ~x =
  Core_unix.mkdir_p dir;
  let csv = dir ^/ "predict.csv"
  and out = dir ^/ "predictions.txt"
  and log = dir ^/ "predict.log" in
  write_csv csv ~x ~y:None;
  run
    [ "task=predict"
    ; "data=" ^ csv
    ; "input_model=" ^ model
    ; "output_result=" ^ out
    ; "verbosity=-1"
    ]
    ~log;
  let ps = In_channel.read_lines out |> List.map ~f:Float.of_string |> Array.of_list in
  if Array.length ps <> Array.length x
  then
    raise_s
      [%message
        "lightgbm returned the wrong number of predictions" (Array.length ps : int)];
  ps
;;

let%expect_test "CSV rows: label first, then features" =
  let path = Filename_unix.temp_file "fade" ".csv" in
  write_csv path ~x:[| [| 1.5; -2. |]; [| 0.; 1e-7 |] |] ~y:(Some [| true; false |]);
  print_string (In_channel.read_all path);
  Sys_unix.remove path;
  [%expect
    {|
    1,1.5,-2
    0,0,1e-07
    |}]
;;
