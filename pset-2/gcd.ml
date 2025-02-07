(* a - Euclid's Original version based on subtraction *)
let rec gcd1 (a : int) (b : int) : int =
  if a = b then a
  else if a > b then gcd1 (a - b) b
  else gcd1 a (b - a)
;;

(* b - An updated version of Euclid's original version *)

let rec gcd2 (a : int) (b : int) : int =
  if b = 0 then a
  else gcd2 b (a mod b)
;;
