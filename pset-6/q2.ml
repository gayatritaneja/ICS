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

let p_count_1 (n: int) : int  =
  find_length (partitions n)
;;

let rec p_count_helper (n: int) (k: int) =
  if n = 0 then 1
  else if n < 0 || k = 0 then 0
  else (p_count_helper (n - k) k) + (p_count_helper n (k - 1))
;;

let p_count_2 (n: int) = p_count_helper n n;;

(* Measure the time taken by a function *)
let time_function f x =
  let start_time = Sys.time () in
  let result = f x in
  let end_time = Sys.time () in
  (result, end_time -. start_time)

(* Collect data points for p_count_1 *)
let collect_p1_data max_n =
  let rec collect n acc =
    if n > max_n then acc
    else
      let (_, time) = time_function p_count_1 n in
      collect (n + 1) ((n, time) :: acc)
  in
  List.rev (collect 1 [])

(* Collect data points for p_count_2 *)
let collect_p2_data max_n =
  let rec collect n acc =
    if n > max_n then acc
    else
      let (_, time) = time_function p_count_2 n in
      collect (n + 1) ((n, time) :: acc)
  in
  List.rev (collect 1 [])

(* Print collected data *)
let print_data_points data =
  List.iter
    (fun (n, time) -> Printf.printf "n = %d, time = %f seconds\n" n time)
    data

(* Example: Collect and print data points for n = 1 to 20 *)
let () =
  let max_n = 50 in
  Printf.printf "Data points for p_count_1:\n";
  let p1_data = collect_p1_data max_n in
  print_data_points p1_data;

