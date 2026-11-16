open! Core

type t = { db : Sqlite3.db }

let check t rc ~what =
  match (rc : Sqlite3.Rc.t) with
  | OK | DONE | ROW -> ()
  | rc ->
    raise_s
      [%message
        "sqlite error" (what : string) (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg t.db)]
;;

let exec_sql t sql = check t (Sqlite3.exec t.db sql) ~what:sql

let open_ path =
  Core_unix.mkdir_p (Filename.dirname path);
  let t = { db = Sqlite3.db_open path } in
  (* WAL lets a reader (fade report) run while fade fetch is writing. *)
  exec_sql t "PRAGMA journal_mode=WAL";
  exec_sql t "PRAGMA synchronous=NORMAL";
  List.iter Schema.statements ~f:(exec_sql t);
  t
;;

let close t = ignore (Sqlite3.db_close t.db : bool)

let transaction t ~f =
  exec_sql t "BEGIN";
  match f () with
  | x ->
    exec_sql t "COMMIT";
    x
  | exception exn ->
    exec_sql t "ROLLBACK";
    raise exn
;;

let with_stmt t sql params ~f =
  let stmt = Sqlite3.prepare t.db sql in
  (* [Exn.protect] runs [finally] whether [f] returns or raises, so statements are
     always finalized. *)
  Exn.protect
    ~f:(fun () ->
      List.iteri params ~f:(fun i p -> check t (Sqlite3.bind stmt (i + 1) p) ~what:sql);
      f stmt)
    ~finally:(fun () -> ignore (Sqlite3.finalize stmt : Sqlite3.Rc.t))
;;

let iter t sql params ~f =
  with_stmt t sql params ~f:(fun stmt ->
    let rec loop () =
      match Sqlite3.step stmt with
      | ROW ->
        f (Sqlite3.row_data stmt);
        loop ()
      | DONE -> ()
      | rc -> check t rc ~what:sql
    in
    loop ())
;;

let exec t sql params = iter t sql params ~f:(fun _ -> ())

let rows t sql params =
  let acc = ref [] in
  iter t sql params ~f:(fun r -> acc := r :: !acc);
  List.rev !acc
;;

let count t sql params =
  match rows t sql params with
  | [ [| Sqlite3.Data.INT n |] ] -> Int64.to_int_exn n
  | _ -> 0
;;

let get_meta t key =
  match rows t "SELECT value FROM meta WHERE key = ?" [ TEXT key ] with
  | [ [| Sqlite3.Data.TEXT v |] ] -> Some v
  | _ -> None
;;

let set_meta t key value =
  exec
    t
    "INSERT INTO meta(key, value) VALUES (?, ?) ON CONFLICT(key) DO UPDATE SET value = \
     excluded.value"
    [ TEXT key; TEXT value ]
;;
