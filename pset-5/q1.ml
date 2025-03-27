module type poly_sig = sig
  type t
  
  val of_list : float list -> t
  val to_string : t -> string
  val add : t -> t -> t
  val subtract : t -> t -> t
  val mult : t -> t -> t
  val derivative : t -> t
  val degree : t -> int
  val eval : float -> t -> float
  val div : t -> t -> t * t
end
module Polynomial : poly_sig = struct

  type t = (int * float) list

  let rec range n =
    if n <= 0 then []  
    else range (n - 1) @ [n]
  ;;

  let term_to_string (exp, coef) =
    if coef = 0.0 then ""
    else if exp = 0 then Printf.sprintf "%.1f" coef 
    else if exp = 1 then Printf.sprintf "%.1fx" coef 
    else Printf.sprintf "%.1fx^%d" coef exp
  ;;

  let to_string poly =
    let terms = List.filter (fun (e, c) -> c <> 0.0) poly |> List.map term_to_string in
    match terms with
    | [] -> "0"  
    | _ -> String.concat " + " terms  
  ;;

  let rec pair_with_exponents (powers: int list) (coeffs: float list) : t =
    match powers, coeffs with
    | [], [] -> [] 
    | p :: ps, x :: xs -> (p, x) :: pair_with_exponents ps xs  
    | _, _ -> failwith "Lists must have the same length"  
  ;;

  let of_list (l1: float list) : (int * float) list =
    let len = (List.length l1) in 
    let powers = range len in
    pair_with_exponents powers l1 
  ;;

  let rec add (p1 : t) (p2 : t) : t = 
    match p1, p2 with
    | [], [] -> []
    | [], _ -> p2 
    | _, [] -> p1
    | (e1, f1):: ps , (e2, f2):: xs ->
      if e1 = e2 then (e1, (f1 +.f2)) :: add ps xs
      else if e1 < e2 then (e1, f1) :: add ps p2
      else (e2, f2) :: add p1 xs
  ;;

  let rec subtract (p1 : t) (p2 : t) : t =
    match p1, p2 with
    | [], [] -> []
    | [], (e2, f2)::xs -> 
      let coef = 0.0 -. f2 in
      if coef = 0.0 then subtract [] xs else (e2, coef) :: subtract [] xs
    | _, [] -> p1
    | (e1, f1)::ps, (e2, f2)::xs ->
      if e1 = e2 then 
        let coef = f1 -. f2 in
        if coef = 0.0 then subtract ps xs 
        else (e1, coef) :: subtract ps xs
      else if e1 < e2 then (e1, f1) :: subtract ps p2
      else (e2, -.f2) :: subtract p1 xs
  ;;


  let rec mult_list (a: (int * float)) (l: t) : t = 
    match l with 
    | [] -> [] 
    | x :: xs -> 
      match a, x with 
      (e1, f1), (e2, f2) -> ((e1 + e2), (f1 *. f2)) :: mult_list a xs
  ;;

  let rec mult (p1: t) (p2: t) : t =
    match p1 with
    | [] -> []
    | x :: xs -> add (mult_list x p2) (mult xs p2)
  ;;

  let rec derivative (p: t) : t =
    match p with
    | [] -> []
    | (e, f) :: t -> (e - 1, (float_of_int e) *. f) :: derivative t
  ;;

  let rec degree (p: t) : int =
    match p with
    | [] -> -1  
    | (e, f) :: t ->
      if f = 0.0 then degree t 
      else max e (degree t)
  ;;


  let rec exp (f: float) (e: int): float =
    match e with
    | 0 -> 1.0
    | _ -> f *. (exp f (e - 1))
  ;;

  let rec eval (x: float) (p: t): float =
    match p with
    | [] -> 0.0
    | (e, f) :: t ->
      (f *.  (exp x e)) +. eval x t
  ;;

  let first_quotient_term (p1: t) (p2: t) : (int * float) option =
    (* If p1 < p2, division stops *)
    if p1 = [] || degree p1 < degree p2 then None  
    else
      let (e1, c1) = List.hd p1 in  (* Highest-degree term of p1 *)
      let (e2, c2) = List.hd p2 in  (* Highest-degree term of p2 *)
      Some (e1 - e2, c1 /. c2) 
  ;;

  let rec div (p1: t) (p2: t) : t * t =
    if p2 = [] then failwith "Cannot divide by zero polynomial"
    (* Base case: remainder = p1 *)
    else if p1 = [] || degree p1 < degree p2 then ([], p1)  
    else
      (* No valid quotient term, return remainder *)
      match first_quotient_term p1 p2 with
      | None -> ([], p1) 
      | Some qt ->
        (* Multiply divisor by quotient term *)
        let subtrahend = mult_list qt p2 in  
        (* Subtract from dividend *)
        let new_dividend = subtract p1 subtrahend in  
        let (quotient_rest, remainder) = div new_dividend p2 in
        (* Append quotient term *)
        (qt :: quotient_rest, remainder)  
  ;;
end
