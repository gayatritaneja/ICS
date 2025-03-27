type nat = Zero | Succ of nat
type sign = Plus | Minus
type rational = (sign * nat * nat)

let rec nat_to_int (x : nat) : int =
  match x with
  | Zero -> 0
  | Succ Zero -> 1
  | Succ x’ -> 1 + nat_to_int x’
;;
(* Defining Subtraction *)
let rec minus (n: nat) (m: nat): nat option =
  match (n, m) with
  | (_, Zero) -> Some n
  | (Zero, _ ) -> None
  | (Succ n’, Succ m’) -> minus n’ m’
;;
(* Defining modulus to calculate gcd using divmod defined in class *)
let rec modulus (n : nat) (m: nat) : nat option =
  match (n, m) with
  | (_, Zero) -> None
  | (Zero, _) -> Some Zero
  | _ ->
  match (minus n m) with
  | None -> Some n
  | Some diff ->
  match (modulus diff m) with
  | None -> None
  | Some r -> Some r
;;
(* Defining gcd using Euclid’s method *)
let rec gcd (m : nat) (n: nat) : nat =
match n with
  | Zero -> m
  | _ ->
  match modulus m n with
  | None -> m
  | Some r -> gcd n r
;;
(* Defining nat division which was also defined in class *)
let rec div (n: nat) (m: nat) : nat option =
  match (n, m) with
  | (_, Zero) -> None
  | (Zero, _) -> Some Zero
  | (Succ n’, Succ m’) ->
  match (minus n m) with
  | None -> Some Zero
  | Some diff ->
  match div diff m with
  | Some q -> Some(Succ q)
  | None -> None
;;
(* Normalizing the rational number *)
let normalise (n : nat) (d : nat) : (nat * nat) =
  let g = gcd n d in
  (* If gcd is 1, return the numerator and denominator *)
  if g = Succ(Zero) then (n, d)
  else
  (* If gcd is not 1, then divide numerator and denominator by the gcd *)
  match (div n g, div d g) with
  | (Some n’, Some d’) -> (n’, d’)
  | _ -> (Zero, Zero)
;;
(* Making a Rational Number *)
let rec make_rational (s: sign) (n: nat) (d: nat) : rational option =
  match d with
  (* Dealing with denominator being zero *)
  | Zero -> None
  | _ ->
  (* Normalising it so it’s in the simplest form *)
  let (n’, d’) = normalise n d in
  (* Won’t happen but dealing with zero as denominator after normalizing *)
  if d’ = Zero then None
  else Some (s, n’, d’)
;;
(* String Representation of rational numbers *)
let rational_to_string (r : rational) : string =
  match r with
  | (Plus, n, d) ->
  Printf.sprintf "%d/%d" (nat_to_int n) (nat_to_int d)
  | (Minus, n, d) ->
  Printf.sprintf "-%d/%d" (nat_to_int n) (nat_to_int d)
;;

let rec int_to_nat (n : int) : nat =
  if n = 0 then Zero else Succ (int_to_nat (n - 1))
;;
(* _________________________________________________________________________ *)
(* Defining addition for nats *)
let rec plus (n: nat) (m: nat) : nat =
  match n with
  | Zero -> m
  | Succ n’ -> Succ (plus n’ m)
;;
(* Defining multiplication for nats *)
let rec mult (n: nat) (m: nat) : nat =
  match n with
  | Zero -> Zero
  | Succ n’ -> plus (mult n’ m) m
;;
let rec nat_minus (n: nat) (m: nat): nat =
  match (n, m) with
  | (_, Zero) -> n
  | (Succ n’, Succ m’) -> nat_minus n’ m’
;;
(* Greater than -> returns true when a is greater than b *)
let rec greater_than (a: nat) (b: nat): bool =
  match (a, b) with
  | (Zero, Zero) -> false
  | (Zero, Succ b’) -> false
  | (Succ a’, Zero) -> true
  | (Succ a’, Succ b’) -> greater_than a’ b’
;;
(* Adding rationals *)
let plus_rational (p: rational) (q: rational) : rational option =
  match (p, q) with
  | (s1, n1, d1), (s2, n2, d2) ->
  if s1 = s2 then
  (* Same sign -> Add the numerators *)
  let num1 = mult n1 d2 in
  let num2 = mult n2 d1 in
  let num = plus num1 num2 in
  let den = mult d1 d2 in
  make_rational s1 num den
  else
  (* Different signs -> Subtract the numerators *)
  let num1 = mult n1 d2 in
  let num2 = mult n2 d1 in
  let den = mult d1 d2 in
  if num1 = num2 then
  (* num1 - num2 = 0 when they are equal *)
  make_rational Plus Zero den
  else if greater_than num1 num2 then
  (* if num1 > num2 then num1 - num2 gives a positive output *)
  let num = nat_minus num1 num2 in
  let den = mult d1 d2 in
  make_rational s1 num den
  else
  (* if num1 < num2 then num2 - num1 gives a positive
  output and the sign changes to accomodate for it *)
  let num = nat_minus num2 num1 in
  let den = mult d1 d2 in
  let new_sign = if s1 = Plus then Minus else Plus in
  make_rational new_sign num den
;;
(* Subtracting rationals *)
let minus_rational (p: rational) (q: rational) : rational option =
  match (p, q) with
  | (s1, n1, d1), (s2, n2, d2) ->
  if s1 != s2 then
  (* Different sign -> Add the numerators *)
  let num1 = mult n1 d2 in
  let num2 = mult n2 d1 in
  let num = plus num1 num2 in
  let den = mult d1 d2 in
  make_rational s1 num den
  else
  (* Same signs -> Subtract the numerators *)
  let num1 = mult n1 d2 in
  let num2 = mult n2 d1 in
  let den = mult d1 d2 in
  if num1 = num2 then
  (* nnum1 - num2 = 0 when num1 = num 2 *)
  make_rational Plus Zero den
  else if greater_than num1 num2 then
  (* if num1 > num2 then num1 - num2 gives a positive output *)
  let num = nat_minus num1 num2 in
  let den = mult d1 d2 in
  make_rational s1 num den
  else
  (* if num1 < num2 then num2 - num1 gives a
  positive output and the sign changes to accomodate for it *)
  let num = nat_minus num2 num1 in
  let den = mult d1 d2 in
  let new_sign = if s1 = Plus then Minus else Plus in
  make_rational new_sign num den
;;
(* Multiplying rationals *)
let multiply_rational (p: rational) (q: rational) : rational option =
  match (p, q) with
  | (s1, n1, d1), (s2, n2, d2) ->
  (* Same sign -> Positive output *)
  if s1 = s2 then
  (* Multiply numerators *)
  let num = mult n1 n2 in
  (* Multiply denominators *)
  let den = mult d1 d2 in
  (* make the results a rational *)
  make_rational Plus num den
  else
  (* Different sign -> Negative output *)
  let num = mult n1 n2 in
  let den = mult d1 d2 in
  make_rational Minus num den
;;
(* Dividing rationals *)
let divide_rational (p: rational) (q: rational) : rational option =
(* Same as multiply_rational but
multiplying p with the reciprocal of q to get p/q *)
  match (p, q) with
  | (s1, n1, d1), (s2, n2, d2) ->
  if s1 = s2 then
  let num = mult n1 d2 in
  let den = mult d1 n2 in
  make_rational Plus num den
  else
  let num = mult n1 d2 in
  let den = mult d1 n2 in
  make_rational Minus num den
;;
(* __________________________________________________________________________________ *)
(* absolute value of rational number *)
let abs_rational (r: rational) : rational =
  match r with
  | (Plus, n, d) -> (Plus, n, d)
  | (Minus, n, d) -> (Plus, n, d)
;;
(* Less than equal to for nat *)
let rec less_than_equal_to_nat (a: nat) (b: nat): bool =
  match (a, b) with
  | (Zero, Zero) -> true
  | (Zero, Succ b’) -> true
  | (Succ a’, Zero) -> false
  | (Succ a’, Succ b’) -> less_than_equal_to_nat a’ b’
;;
(* Less than equal to for rational *)
let less_than_equal_to (a: rational) (b: rational) : bool =
  match a, b with
  | (s1, n1, d1), (s2, n2, d2) ->
  if less_than_equal_to_nat (mult n1 d2) (mult n2 d1) then true
  else false
;;
let square_rational (a: rational) : rational =
  match a with (s1, n1, d1) ->
  let num = mult n1 n1 in
  let den = mult d1 d1 in
  (Plus, num, den)
;;
let error_true (a: rational) (r: rational) (epsilon: rational) : bool =
  let a_squared = square_rational a in
  match minus_rational r a_squared with
  | Some diff ->
  let abs_diff = abs_rational diff in
  let result = less_than_equal_to abs_diff epsilon in
  result
  | None ->
  false
;;
(* Recursive Function for Square Root Approximation *)
let rec approx_sqrt_2 (r: rational) (e: rational) (a: rational) : rational =
  if error_true a r e then a
  else
  (* r / a *)
  let quotient = match divide_rational r a with
  | Some q -> q
  | None -> failwith "Error in dividing r by a"
  in
  (* r/a + a *)
  let sum = match plus_rational quotient a with
  | Some s -> s
  | None -> failwith "Error in adding quotient and a"
  in
  (* 2 = 2/1 *)
  let two = match make_rational Plus (int_to_nat 2) (int_to_nat 1) with
  | Some t -> t
  | None -> failwith "Error in creating rational for 2"
  in
  (* (r/a + a) / 2 *)
  let next_a = match divide_rational sum two with
  | Some next_a -> next_a
  | None -> failwith "Error in dividing sum by 2"
  in
  (* Recursive call with the new approximation *)
  approx_sqrt_2 r e next_a
;;
let test_approx_sqrt_2 () =
  (* Change values from here *)
  let r = make_rational Plus (int_to_nat 2) (int_to_nat 1) in
  let epsilon = make_rational Plus (int_to_nat 1) (int_to_nat 100) in
  let a = make_rational Plus (int_to_nat 1) (int_to_nat 1) in
  match (r, epsilon, a) with
  | (Some r, Some epsilon, Some a) ->
  let result = approx_sqrt_2 r epsilon a in
  Printf.printf "Approximation of sqrt(2): %s\n" (rational_to_string result)
;;
test_approx_sqrt_2 ();;
