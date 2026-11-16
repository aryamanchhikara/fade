(lang dune 3.11)

(name fade)

(generate_opam_files true)

(source
 (github aryamanchhikara/fade))

(authors "Aryaman Chhikara")

(maintainers "Aryaman Chhikara")

(license MIT)

(package
 (name fade)
 (synopsis "Out-of-sample test of whether buying NO on Kalshi is +EV after fees")
 (description
  "OCaml backend and Bonsai UI sharing types via js_of_ocaml. Fee-aware calibration, event-clustered bootstrap CIs, maker vs taker returns, frozen-rule out-of-time evaluation.")
 (depends
  (ocaml
   (and
    (>= 5.1.0)
    (< 5.2)))
  (core
   (and
    (>= v0.17)
    (< v0.18)))
  (core_unix
   (and
    (>= v0.17)
    (< v0.18)))
  (async
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_jane
   (and
    (>= v0.17)
    (< v0.18)))
  (bonsai
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_yojson_conv
   (and
    (>= v0.17)
    (< v0.18)))
  yojson
  cohttp-async
  async_ssl
  sqlite3
  digestif
  ezgzip
  (js_of_ocaml
   (and
    (>= 5.1)
    (< 5.7)))
  js_of_ocaml-ppx))
