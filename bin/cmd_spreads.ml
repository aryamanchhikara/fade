open! Core
open Fade_types
module Spreads = Fade_analysis.Spreads

(* `fade spreads`: how often the book was one-sided or wide at a horizon, by category,
   over the sampled markets in the study window. *)

let command =
  Command.basic
    ~summary:"Share of sampled markets with a wide or one-sided book, by category"
    (let%map_open.Command source = Data_source.param
     and frozen = flag "--frozen" (optional string) ~doc:"PATH study frozen rules"
     and max_spread =
       flag "--max-spread" (optional_with_default 5. float) ~doc:"C cents (default 5)"
     and horizon =
       flag
         "--horizon"
         (optional_with_default
            Horizon.H1
            (Arg_type.of_alist_exn
               (List.map Horizon.all ~f:(fun h -> Horizon.to_string h, h))))
         ~doc:"24h|6h|1h (default 1h)"
     in
     fun () ->
       let frozen_path =
         Option.value frozen ~default:(Cmd_report.default_frozen source)
       in
       let study = Cmd_report.load_frozen source ~frozen_path in
       let d = Data_source.load source ~anchor:study.config.anchor in
       let d = Fade_analysis.Report_builder.restrict ~config:study.config d in
       printf
         "Book at %s before close, sampled markets settled %s to %s\n\n"
         (Horizon.to_string horizon)
         study.config.data_since
         study.config.data_until;
       printf
         "| Category | Quoted | One-sided | Two-sided, spread > %.0f¢ | Not tight \
          (either) |\n\
          | --- | ---: | ---: | ---: | ---: |\n"
         max_spread;
       List.iter (Spreads.by_category ~max_spread ~horizon d) ~f:(fun r ->
         printf
           "| %s | %d | %.1f%% | %.1f%% | %.1f%% |\n"
           r.category
           r.quoted
           (100. *. Spreads.Row.share r.one_sided r.quoted)
           (100. *. Spreads.Row.share r.wide (Spreads.Row.two_sided r))
           (100. *. Spreads.Row.not_tight r)))
;;
