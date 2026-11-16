open! Core

let command =
  Command.basic
    ~summary:"Write the synthetic fixture dataset (deterministic; safe to commit)"
    (let%map_open.Command out =
       flag
         "--out"
         (optional_with_default "test/fixtures/synthetic.json" string)
         ~doc:"PATH where to write (default test/fixtures/synthetic.json)"
     in
     fun () ->
       let d = Fade_synth.generate () in
       Out_channel.write_all out ~data:(Fade_synth.to_json_string d);
       printf
         "wrote %s: %d markets, %d snapshots, %d trades\n"
         out
         (List.length d.markets)
         (List.length d.snapshots)
         (List.length d.trades))
;;
