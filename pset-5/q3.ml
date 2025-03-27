(* Checks if an element x is in the given list *)
let rec search (l1: string list) (x: string) : bool =
  match l1 with
  | [] -> false
  | h :: t -> if h = x then true else search t x
;;

(* Converts a string to a list to make comparisons more convveninent *)
let string_to_list (s1: string): char list = 
  s1 |> String.to_seq |> List.of_seq
;;

(* Returns true iff there is only a single character difference in s1 and s2 *)
let rec compare (s1: char list) (s2: char list) (found_diff: bool): bool =
  match (s1, s2) with
  | ([], []) -> found_diff
  | ([], _) -> false
  | (_, []) -> false
  (* if h1 = h2 i.e the characters are not different then recursion continues *)
  (* if h1 is not equal to h2 and found_diff is already true, then returns false
  because there is more than one difference *)
  (* if h1 is not equal to h2 and found_diff is false, then switches the value 
  of found_diff to true for the next recursive call and continues *)
  | (h1::t1), (h2::t2) -> 
    match h1 = h2 with
    | true ->  compare t1 t2 found_diff
    | false -> 
      if found_diff = true then false
      else compare t1 t2 true
;;

(* Uses search to check for duplicacy, if there is a duplicate element 
then returns false, else passes each pair to compare to see if there is 
only 1 difference 
If compare returns false then returns false, else continues checking
further pairs *)
let rec connected (l : string list) : bool =
  match l with
  | [] -> true
  | h1 :: [] -> true
  | h1 :: h2 :: t -> 
    if search (h2 :: t) h1 = true then false 
    else
    match compare (string_to_list h1) (string_to_list h2) false with
    | false -> false
    | true -> connected (h2 :: t)
;;

(* Change value of lst for other test cases *)
let lst = ["aa";"ab";"bb";"ba"] in
print_string (string_of_bool (connected lst))