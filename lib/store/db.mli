(** The SQLite cache. *)

open! Core

type t

(** Open (creating the file, its directory and the schema if needed). *)
val open_ : string -> t

val close : t -> unit

(** Run [f] inside one transaction; much faster for bulk upserts. *)
val transaction : t -> f:(unit -> 'a) -> 'a

val get_meta : t -> string -> string option
val set_meta : t -> string -> string -> unit

(** Execute a statement with positional parameters, ignoring any rows. *)
val exec : t -> string -> Sqlite3.Data.t list -> unit

(** Run a query, calling [f] on every row. *)
val iter : t -> string -> Sqlite3.Data.t list -> f:(Sqlite3.Data.t array -> unit) -> unit

(** Convenience: run a query returning rows. *)
val rows : t -> string -> Sqlite3.Data.t list -> Sqlite3.Data.t array list

(** Convenience: run a query returning one integer. *)
val count : t -> string -> Sqlite3.Data.t list -> int
