let rec gcd1 (a : int) (b : int) : int =
(* Base case: if both numbers are equal, return one of them *)
if a = b then a
(* Reduce the larger number by subtracting the smaller one from it recursivley *)
else if a > b then gcd1 (a - b) b
else gcd1 a (b - a)
;;


let rec gcd2 (a : int) (b : int) : int =
(* Base case: if b is 0, return a *)
if b = 0 then a
(* replace (a, b) with (b, a mod b) recursively*)
else gcd2 b (a mod b)
;;


let rec extended_gcd (a : int) (b : int) : (int * int * int) =
(* Base case: x = 1, y = 0 *)
if b = 0 then (a, 1, 0)
else
let (gcd, x1, y1) = extended_gcd b (a mod b) in
(* Returning a tuple containing the gcd, x and y*)
(gcd, y1, x1 - (a / b) * y1)
;;
