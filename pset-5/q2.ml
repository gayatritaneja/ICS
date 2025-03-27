(* print_int_list and print_pair_list made for troubleshooting, can be ignored *)
let print_int_list lst =
  List.iter (fun x -> print_int x; print_string " ") lst;
  print_newline ()
;;

let rec print_pair_list lst =
  match lst with
  | [] -> print_newline () 
  | [(a, b)] -> Printf.printf "(%d,%d)\n" a b 
  | (a, b) :: xs -> 
      Printf.printf "(%d,%d) " a b; 
      print_pair_list xs
;;

(* prints a numbers from a to b - both inclusive*)
let rec range (a: int) (b: int): int list =
  if a > b then []
  else (a) :: range (a + 1) b
;;

(* indexes the list. For example, [1; 3; 2; 7] becomes [(1, 0); (3, 1), (2, 2), (7, 3)] *)
let rec index (l: int list) (i: int list) : (int * int) list =
  match l, i with
  | [], [] -> [] 
  | p :: ps, x :: xs -> (p, x) :: index ps xs  
  | _, _ -> failwith "Lists must have the same length"  
;;


(* Regular filter function *)
let rec filter (f: 'a -> 'b -> bool) (l: 'a list) (i: 'b): 'a list =
  match l with
  | [] -> []
  | h :: t ->
    match f h i with
    | true -> h :: filter f t i
    | false -> filter f t i
  ;;

(* Returns all elements of the indexed list before a given index *)
let is_lesser_index (h: (int * int)) (i: int): bool = 
  match h with
  | (a, b) -> if b <= i then true else false
;;

(* Returns all elements of the indexed list after a given index *)
let is_greater_index (l: (int * int)) (i: int): bool = 
  match l with
  | (a, b) -> if b > i then true else false
;;

(* Returns all elements of the indexed lesser than the indexed value *)
let is_lesser (l: (int * int)) (i: int): bool = 
  match l with
  | (a, b) -> if a <= i then true else false
;;


(* Returns all elements of the indexed list greater than the given value *)
let is_greater (l: (int * int)) (i: int): bool = 
  match l with
  | (a, b) -> 
    if a > i then true else false
;;

(* Checks if, for a given element, all elements after it are greater than the element 
and all elements before it are lesser than it and returns true *)
let rec is_valid (h: (int * int)) (l: (int * int) list) : bool =
  match h with 
  | (i, j) ->
    (* i is index and j is actual value *)
    let less_index = filter (is_lesser_index) l j in
    let more_index = filter (is_greater_index) l j in 
    let less = filter (is_lesser) l i in
    let more = filter (is_greater) l i in
    if less = less_index && more = more_index then true else false
  ;;

let rec partitioned (l: int list) : bool =
  let len = List.length l in 
  let i = range 0 (len - 1) in
  let indexed = index l i in

  let rec check_all lst =
    match lst with
    | [] -> false
    | h1 :: t1 ->
        if is_valid h1 indexed then true
        else check_all t1 
  in
  check_all indexed
;;

(* Change value of lst for other test cases *)
let lst = [19;13;16;15;25;19;22] in
print_string (string_of_bool (partitioned lst));;