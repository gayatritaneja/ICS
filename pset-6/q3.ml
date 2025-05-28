let rec find_length (l1: (int list) list) = 
  match l1 with
  | [] -> 0
  | h :: t -> 1 + find_length t
;;

let rec process (f: 'a -> 'b -> 'a) (acc: 'a) (l1: 'b list) : 'a =
  match l1 with
  | [] -> acc 
  | x :: xs -> process f (f acc x) xs
;;

let rec is_not_in (l1: int list) (l2: (int list)list) =
  match l2 with
  | [] -> true
  | h :: t -> 
    match (h = l1) with
    | true -> false
    | false -> is_not_in l1 t
;;

let rec find_parts (n: int) (i: int) (acc: (int list)list): (int list) list = 
    match (i > n) with
    | true -> acc
    | false -> 
      let pair = [i; n-i] in
      let rev_pair = [n - i; i] in
      if is_not_in pair acc && is_not_in rev_pair acc then
      find_parts n (i + 1) (pair :: acc)
      else find_parts n (i + 1) acc
;;

let rec append_parts (sub_partitions: (int list) list) (first: int) (result: (int list) list) :  (int list) list =
  match sub_partitions with
  | [] -> result
  | sub :: rest ->
    let new_partition = List.sort compare (first :: sub) in
    if is_not_in new_partition result then
      append_parts rest first (new_partition :: result)
    else
      append_parts rest first result
;;


let rec partitions (n: int) : (int list) list =
  match n with
  | 0 -> []
  | 1 -> [[1]]
  | _ ->
    let parts = find_parts n 1 [] in
    process
      (fun result pair ->
        match pair with
        | [first; remaining] ->
          if remaining = 0 then
            [first] :: result
          else
            let sub_partitions = partitions remaining in
            let updated_result = append_parts sub_partitions first result in
            updated_result
        | _ -> result)
      [] parts
;;

let rec is_diff_at_least_2 (l1: int list) : bool =
  match l1 with
  | [] -> true
  | [a] -> true
  | h1 :: h2 :: t -> 
    match (h2 - h1 >= 2) with
    | true -> is_diff_at_least_2 (h2 :: t)
    | false -> false
  ;;

let rec is_1_or_4_mod_5 (l1: int list) : bool =
  match l1 with
  | [] -> true
  | h :: t -> 
    (1 = h mod 5 || 4 = h mod 5) && is_1_or_4_mod_5 t
  ;;

let rec filter (f: int list -> bool) (l1: (int list) list): (int list) list =
  match l1 with
  | [] -> []
  | h :: t -> 
    if f h = false then filter f t
    else h :: filter f t
  ;;

let rec check_rr_identity (n: int) : bool =
  let l1 = partitions n in
  let len1 = find_length (filter (is_1_or_4_mod_5) l1) in
  let len2 = find_length (filter (is_diff_at_least_2) l1) in
  match (len1 = len2) with
  | true -> true
  | false -> false
;;


print_string(string_of_bool (check_rr_identity 7))