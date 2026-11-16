(** Small statistics toolkit. Everything is deterministic given its inputs. *)

open! Core
open Fade_types

(** Wilson score interval for a binomial proportion at 95%. [successes] may be
    fractional (contract-weighted). Returns [{lo = 0; hi = 1}] when [n = 0]. *)
val wilson : successes:float -> n:float -> Ci.t

(** [percentile sorted p] for [p] in [0, 1], linear interpolation between order
    statistics. [sorted] must be sorted ascending and non-empty. *)
val percentile : float array -> float -> float

(** One cluster's contribution to a ratio estimate [sum num / sum den]. For a
    per-market mean, [num] is the sum of returns and [den] the number of markets. *)
type cluster =
  { num : float
  ; den : float
  }

(** Percentile bootstrap CI for [sum num / sum den], resampling whole clusters with
    replacement. Clusters are events, so markets inside one event move together.
    Returns [None] if there are no clusters. *)
val cluster_bootstrap
  :  rng:Random.State.t
  -> resamples:int
  -> cluster array
  -> Ci.t option
