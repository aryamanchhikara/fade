open! Core

(* Logistic regression, written out by hand.

   THE MODEL. For a row with feature vector x (d numbers), the predicted probability
   that the outcome is 1 is

     p = sigma(b + w . x),    sigma(z) = 1 / (1 + e^(-z))

   [w] has one weight per feature, [b] is the intercept. sigma squashes any real number
   into (0, 1); z = b + w . x is the "log-odds", log(p / (1 - p)).

   THE LOSS. We want weights that make the observed outcomes y (0 or 1) likely. The
   average log loss (negative log-likelihood) is

     L(w, b) = -(1/n) sum_i [ y_i log p_i + (1 - y_i) log (1 - p_i) ]
               + (lambda / 2) |w|^2

   The second term is L2 regularization ("ridge"): it charges for large weights, which
   keeps the model from fitting noise. The intercept is not penalized.

   THE GRADIENT. The slope of L in each weight direction. A short calculation (using
   sigma'(z) = sigma(z) (1 - sigma(z))) gives a strikingly simple form:

     dL/dw_j = (1/n) sum_i (p_i - y_i) x_ij  + lambda w_j
     dL/db   = (1/n) sum_i (p_i - y_i)

   (p_i - y_i) is the prediction error on row i; each weight moves in proportion to
   how much its feature correlates with the errors.

   GRADIENT DESCENT. Start at w = 0, b = 0 and repeatedly step downhill:

     w <- w - eta * dL/dw,    b <- b - eta * dL/db

   L is convex (bowl-shaped), so there is one minimum and descent reaches it if the
   step [eta] is small enough. "Small enough" is eta <= 1 / M, where M bounds the
   curvature of L: the second derivative of log loss is at most 1/4 per row, so
   M <= 1/4 * (1 + sum_j mean(x_j^2)) + lambda. With standardized features (mean 0,
   variance 1) that is about (d + 1) / 4 + lambda.

   MOMENTUM (Nesterov). Plain descent zig-zags across long narrow valleys. Momentum
   keeps a running velocity: look ahead to where the last step was heading, take the
   gradient there, and add it to the velocity. Same answer, far fewer steps.

   NEWTON / IRLS, the reference. Newton's method uses the curvature too: the Hessian
   H = (1/n) X^T S X + lambda I', with S = diag(p_i (1 - p_i)), and jumps straight to
   the minimum of the local quadratic: theta <- theta - H^-1 grad. For logistic
   regression this is "iteratively reweighted least squares". It converges in a handful
   of steps but needs a d x d solve each time; gradient descent only needs gradients,
   which is why it scales and why it is worth learning. The expect test checks that
   the two agree. *)

type t =
  { intercept : float
  ; weights : float array
  }
[@@deriving sexp]

let sigmoid z =
  (* Written to avoid overflow: e^(-z) for very negative z would be huge. *)
  if Float.( >= ) z 0.
  then 1. /. (1. +. Float.exp (-.z))
  else (
    let e = Float.exp z in
    e /. (1. +. e))
;;

let linear t (x : float array) =
  let z = ref t.intercept in
  Array.iteri x ~f:(fun j v -> z := !z +. (t.weights.(j) *. v));
  !z
;;

let predict t x = sigmoid (linear t x)

(* Mean log loss of probabilities [p] for outcomes [y], clipped so that a confident
   wrong prediction costs a lot but not infinity. *)
let log_loss ~(p : float array) ~(y : bool array) =
  let eps = 1e-12 in
  let n = Array.length p in
  let total = ref 0. in
  Array.iteri p ~f:(fun i pi ->
    let pi = Float.min (1. -. eps) (Float.max eps pi) in
    total := !total -. if y.(i) then Float.log pi else Float.log (1. -. pi));
  !total /. Float.of_int n
;;

(* The loss and its gradient at [t], for rows [x] and outcomes [y]. *)
let loss_and_gradient t ~(x : float array array) ~(y : bool array) ~lambda =
  let n = Array.length x in
  let d = Array.length t.weights in
  let grad_w = Array.create ~len:d 0. in
  let grad_b = ref 0. in
  let loss = ref 0. in
  for i = 0 to n - 1 do
    let p = predict t x.(i) in
    let err = p -. if y.(i) then 1. else 0. in
    grad_b := !grad_b +. err;
    Array.iteri x.(i) ~f:(fun j v -> grad_w.(j) <- grad_w.(j) +. (err *. v));
    let p = Float.min (1. -. 1e-12) (Float.max 1e-12 p) in
    loss := !loss -. if y.(i) then Float.log p else Float.log (1. -. p)
  done;
  let nf = Float.of_int n in
  let penalty = ref 0. in
  Array.iteri grad_w ~f:(fun j g ->
    grad_w.(j) <- (g /. nf) +. (lambda *. t.weights.(j));
    penalty := !penalty +. (t.weights.(j) *. t.weights.(j)));
  (!loss /. nf) +. (lambda /. 2. *. !penalty), grad_w, !grad_b /. nf
;;

(* Just the gradient (no logs, so cheaper than [loss_and_gradient]): the inner loop of
   gradient descent only needs the direction downhill. *)
let gradient t ~(x : float array array) ~(y : bool array) ~lambda =
  let n = Array.length x in
  let d = Array.length t.weights in
  let grad_w = Array.create ~len:d 0. in
  let grad_b = ref 0. in
  for i = 0 to n - 1 do
    let row = x.(i) in
    let z = ref t.intercept in
    for j = 0 to d - 1 do
      z := !z +. (t.weights.(j) *. row.(j))
    done;
    let err = sigmoid !z -. if y.(i) then 1. else 0. in
    grad_b := !grad_b +. err;
    for j = 0 to d - 1 do
      grad_w.(j) <- grad_w.(j) +. (err *. row.(j))
    done
  done;
  let nf = Float.of_int n in
  for j = 0 to d - 1 do
    grad_w.(j) <- (grad_w.(j) /. nf) +. (lambda *. t.weights.(j))
  done;
  grad_w, !grad_b /. nf
;;

(* Gradient descent with Nesterov momentum. Stops when the gradient is essentially zero
   (we are at the bottom of the bowl) or after [max_iter] steps. [init] starts from an
   earlier solution ("warm start"), e.g. the fit for a nearby L2 strength.

   A note on tolerance: when features are nearly collinear (the price spline), the bowl
   has long flat directions where the loss barely changes. Descent creeps along them,
   but they are also directions in which the predictions barely change, so a looser
   [tol] stops early without changing what the model predicts in any way that matters. *)
let fit_gd ?(lambda = 0.) ?(max_iter = 20_000) ?(tol = 1e-9) ?init ~x ~y () =
  let d = if Array.is_empty x then 0 else Array.length x.(0) in
  let n = Float.of_int (Array.length x) in
  (* Curvature bound M, and the safe step size eta = 1 / M. *)
  let sum_sq = Array.create ~len:d 0. in
  Array.iter x ~f:(fun row ->
    Array.iteri row ~f:(fun j v -> sum_sq.(j) <- sum_sq.(j) +. (v *. v)));
  let m =
    (0.25 *. (1. +. Array.fold sum_sq ~init:0. ~f:(fun acc s -> acc +. (s /. n))))
    +. lambda
  in
  let eta = 1. /. m in
  let current =
    ref (Option.value init ~default:{ intercept = 0.; weights = Array.create ~len:d 0. })
  in
  let previous = ref !current in
  let rec loop k =
    (* Look-ahead point: current + (k - 1) / (k + 2) * (current - previous). *)
    let mu = Float.of_int (k - 1) /. Float.of_int (k + 2) in
    let ahead =
      { intercept =
          !current.intercept +. (mu *. (!current.intercept -. !previous.intercept))
      ; weights =
          Array.mapi !current.weights ~f:(fun j w ->
            w +. (mu *. (w -. !previous.weights.(j))))
      }
    in
    let gw, gb = gradient ahead ~x ~y ~lambda in
    (* One step downhill from the look-ahead point. *)
    let next =
      { intercept = ahead.intercept -. (eta *. gb)
      ; weights = Array.mapi ahead.weights ~f:(fun j w -> w -. (eta *. gw.(j)))
      }
    in
    previous := !current;
    current := next;
    let gnorm =
      Float.sqrt ((gb *. gb) +. Array.fold gw ~init:0. ~f:(fun a g -> a +. (g *. g)))
    in
    if Float.( < ) gnorm tol || k >= max_iter then k else loop (k + 1)
  in
  let iterations = loop 1 in
  !current, iterations
;;

(* Solve A z = r for a small symmetric positive-definite A (Gaussian elimination with
   partial pivoting). *)
let solve (a : float array array) (r : float array) =
  let n = Array.length r in
  let a = Array.map a ~f:Array.copy in
  let r = Array.copy r in
  for col = 0 to n - 1 do
    let pivot = ref col in
    for row = col + 1 to n - 1 do
      if Float.( > ) (Float.abs a.(row).(col)) (Float.abs a.(!pivot).(col))
      then pivot := row
    done;
    Array.swap a col !pivot;
    Array.swap r col !pivot;
    for row = col + 1 to n - 1 do
      let f = a.(row).(col) /. a.(col).(col) in
      for k = col to n - 1 do
        a.(row).(k) <- a.(row).(k) -. (f *. a.(col).(k))
      done;
      r.(row) <- r.(row) -. (f *. r.(col))
    done
  done;
  let z = Array.create ~len:n 0. in
  for row = n - 1 downto 0 do
    let s = ref r.(row) in
    for k = row + 1 to n - 1 do
      s := !s -. (a.(row).(k) *. z.(k))
    done;
    z.(row) <- !s /. a.(row).(row)
  done;
  z
;;

(* Newton's method (IRLS). Parameters are theta = [b; w]; index 0 is the intercept. *)
let fit_irls ?(lambda = 0.) ?(max_iter = 50) ~x ~y () =
  let d = if Array.is_empty x then 0 else Array.length x.(0) in
  let n = Float.of_int (Array.length x) in
  let t = ref { intercept = 0.; weights = Array.create ~len:d 0. } in
  let rec loop k =
    let h = Array.make_matrix ~dimx:(d + 1) ~dimy:(d + 1) 0. in
    let g = Array.create ~len:(d + 1) 0. in
    Array.iteri x ~f:(fun i row ->
      let p = predict !t row in
      let s = p *. (1. -. p) in
      let err = p -. if y.(i) then 1. else 0. in
      let xe = Array.append [| 1. |] row in
      for a = 0 to d do
        g.(a) <- g.(a) +. (err *. xe.(a));
        for b = 0 to d do
          h.(a).(b) <- h.(a).(b) +. (s *. xe.(a) *. xe.(b))
        done
      done);
    for a = 0 to d do
      g.(a) <- g.(a) /. n;
      for b = 0 to d do
        h.(a).(b) <- h.(a).(b) /. n
      done
    done;
    for j = 1 to d do
      g.(j) <- g.(j) +. (lambda *. !t.weights.(j - 1));
      h.(j).(j) <- h.(j).(j) +. lambda
    done;
    let step = solve h g in
    t
    := { intercept = !t.intercept -. step.(0)
       ; weights = Array.mapi !t.weights ~f:(fun j w -> w -. step.(j + 1))
       };
    let size = Array.fold step ~init:0. ~f:(fun a s -> Float.max a (Float.abs s)) in
    if Float.( < ) size 1e-12 || k >= max_iter then k else loop (k + 1)
  in
  let iterations = loop 1 in
  !t, iterations
;;

(* Standardization: subtract each feature's mean and divide by its standard deviation,
   both computed on training rows only. Gradient descent then sees features on one
   scale, which makes the bowl round instead of a long valley. *)
module Scaler = struct
  type t =
    { mean : float array
    ; sd : float array
    }
  [@@deriving sexp]

  let fit (x : float array array) =
    let n = Float.of_int (Array.length x) in
    let d = Array.length x.(0) in
    let mean =
      Array.init d ~f:(fun j -> Array.fold x ~init:0. ~f:(fun a r -> a +. r.(j)) /. n)
    in
    let sd =
      Array.init d ~f:(fun j ->
        let v =
          Array.fold x ~init:0. ~f:(fun a r -> a +. ((r.(j) -. mean.(j)) ** 2.)) /. n
        in
        if Float.( > ) v 1e-18 then Float.sqrt v else 1.)
    in
    { mean; sd }
  ;;

  let apply t row = Array.mapi row ~f:(fun j v -> (v -. t.mean.(j)) /. t.sd.(j))
end

let%expect_test "gradient descent matches Newton (IRLS), with and without L2" =
  (* A small fixed dataset: 2 features, 40 rows, generated by a deterministic rule
     with some noise so that the fit is not perfect. *)
  let rng = Random.State.make [| 7 |] in
  let x =
    Array.init 40 ~f:(fun _ ->
      [| Random.State.float rng 4. -. 2.; Random.State.float rng 2. -. 1. |])
  in
  let y =
    Array.map x ~f:(fun r ->
      let p = sigmoid (0.3 +. (1.2 *. r.(0)) -. (0.8 *. r.(1))) in
      Float.( < ) (Random.State.float rng 1.) p)
  in
  List.iter [ 0.; 0.1 ] ~f:(fun lambda ->
    let gd, _ = fit_gd ~lambda ~x ~y () in
    let nt, _ = fit_irls ~lambda ~x ~y () in
    let show (t : t) =
      sprintf "b=%.6f w=[%.6f; %.6f]" t.intercept t.weights.(0) t.weights.(1)
    in
    let max_diff =
      Float.max
        (Float.abs (gd.intercept -. nt.intercept))
        (Array.fold2_exn gd.weights nt.weights ~init:0. ~f:(fun a u v ->
           Float.max a (Float.abs (u -. v))))
    in
    printf
      "lambda %.1f\n\
      \  gradient descent %s\n\
      \  newton (IRLS)    %s\n\
      \  max |diff| < 1e-6: %b\n"
      lambda
      (show gd)
      (show nt)
      Float.(max_diff < 1e-6));
  [%expect
    {|
    lambda 0.0
      gradient descent b=0.033459 w=[1.439289; -0.417187]
      newton (IRLS)    b=0.033459 w=[1.439289; -0.417187]
      max |diff| < 1e-6: true
    lambda 0.1
      gradient descent b=-0.059410 w=[0.904987; -0.112538]
      newton (IRLS)    b=-0.059410 w=[0.904987; -0.112538]
      max |diff| < 1e-6: true
    |}]
;;
