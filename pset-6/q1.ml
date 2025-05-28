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

(* Returns a list of lists of all the possible unique pairs that add up to n *)
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

(* Generates sub_partitions of *)
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
