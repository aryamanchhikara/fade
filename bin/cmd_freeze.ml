open! Core
open Fade_types

let command =
  Command.basic
    ~summary:"Choose rules on in-sample data only and freeze them with a hash"
    ~readme:(fun () ->
      "Builds only in-sample tables (events fully settled before the cutoff), selects \
       every NO-side cell whose return CI is above zero, and writes the config and rules \
       with a SHA-256 hash. `fade report` refuses a frozen file whose hash no longer \
       matches. Refuses to overwrite an existing file without --force: refreezing after \
       seeing out-of-time results defeats the test.")
    (let%map_open.Command source = Data_source.param
     and out = flag "--out" (optional string) ~doc:"PATH (default rules/frozen.json)"
     and cutoff =
       flag
         "--cutoff"
         (optional string)
         ~doc:
           "YYYY-MM-DD in-sample/out-of-time boundary (default: last settle - 6 months)"
     and min_n = flag "--min-n" (optional_with_default 30 int) ~doc:"N (default 30)"
     and anchor =
       flag
         "--anchor"
         (optional_with_default
            Anchor.Schedule
            (Arg_type.of_alist_exn [ "schedule", Anchor.Schedule; "close", Close ]))
         ~doc:"schedule|close what horizons count back from (default schedule)"
     and contracts =
       flag
         "--contracts"
         (optional_with_default 100. float)
         ~doc:"N order size used for fee rounding (default 100)"
     and resamples =
       flag "--resamples" (optional_with_default 1000 int) ~doc:"N bootstrap resamples"
     and force = flag "--force" no_arg ~doc:" overwrite an existing frozen file" in
     fun () ->
       let out =
         Option.value
           out
           ~default:
             (if Data_source.is_demo source
              then "rules/frozen.demo.json"
              else "rules/frozen.json")
       in
       if Sys_unix.file_exists_exn out && not force
       then (
         eprintf
           "%s already exists. Freezing again after looking at out-of-time results \
            invalidates them; pass --force if you really mean to.\n"
           out;
         exit 1);
       let d = Data_source.load source ~anchor in
       let config =
         Fade_analysis.Report_builder.default_config
           ?cutoff
           ~min_n
           ~anchor
           ~contracts_per_order:contracts
           ~bootstrap_resamples:resamples
           d
       in
       let frozen =
         Fade_analysis.Report_builder.freeze
           ~frozen_at:(Data_source.now_string ())
           ~config
           d
       in
       Core_unix.mkdir_p (Filename.dirname out);
       Out_channel.write_all out ~data:(Fade_analysis.Frozen.to_string frozen);
       printf
         "froze %d rules (from %d in-sample cells with n >= %d) to %s\nsha256 %s\n"
         (List.length frozen.rules)
         frozen.examined
         min_n
         out
         frozen.hash;
       List.iter frozen.rules ~f:(fun r -> printf "  %s\n" (Rule.describe r)))
;;
