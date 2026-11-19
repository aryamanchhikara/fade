(* The library's public face: the database handle plus the dataset loader. [include]
   copies every definition of [Db] into this module, so callers keep writing
   [Fade_store.open_], [Fade_store.exec], ... *)
include Db
module Load = Load
module Pit_load = Pit_load
