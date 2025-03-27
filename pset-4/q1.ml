module Binary = struct
  type bin =
  | E
  | O of bin
  | I of bin
;;
  (* Converting binary to string *)
  let bin_to_string (a: bin) : string = 
    let rec bin_to_string_inner (a: bin) : string = 
      match a with
      | E -> ""
      (* Converting the first bit to a string by concatenating it to the front of the string *)
      (* Then recursively coverting the remaning binary into string *)
      | O a -> bin_to_string_inner a ^ "0" 
      | I a -> bin_to_string_inner a  ^ "1" in 

    if a = E then "E"
    else bin_to_string_inner a
  ;;

  (* Converting String to Binary *)
  let rec string_to_bin (a: string): bin = 
    let len = String.length a in
    if len = 0 then E
    else
      (* Starting from the digit in the ones place or the last digit in a string *)
      (* Converting the rest of the string to bin bit-by-bit recursively *)
      let c = a.[len - 1] in 
      let rest = String.sub a 0 (len - 1) in
      match c with
      | '1' -> I (string_to_bin rest)
      | '0' -> O (string_to_bin rest)
      | _ -> E
  ;;

  (* Converting binary to integer *)
  (* Similar to how it was discussed in class *)
  let rec bin_to_int (a: bin) : int = 
    match a with
    | E -> 0
    | O a' -> 2 * (bin_to_int a')
    | I a' -> (2 * (bin_to_int a')) + 1
  ;;

  (* Converting integer to binary *)
  let rec int_to_bin (x: int) : bin =
    match x with
    | 0 -> E
    | n -> 
      (* If it's even then the first digit would be a zero*)
      if n mod 2 = 0 then O(int_to_bin (n/2))
      else I(int_to_bin (n/2))
  ;;

  (* Successor of *)
  let rec succ_binary (m: bin): bin = 
    match m with
    | E -> I E
    | O m' -> I m'
    (* to replace succ of 1 with 0 followed by succ of the next digit *)
    | I m' -> O (succ_binary m')
  ;;

  (* Adding binary *)
  let rec bin_add (m : bin) (n: bin) : bin = 
    match (m, n) with
    | (a, E) -> a
    | (E, a) -> a
    | (O m', O n') -> O (bin_add m' n')
    | (I m', O n') -> I (bin_add m' n')
    | (O m', I n') -> I (bin_add m' n')
    (* Carry over case *)
    | (I m', I n') -> O (succ_binary (bin_add m' n'))
  ;;
  
  (* Reversing a string *)
    let rec reverse_string (s: string) : string =
    let len = String.length s in
    if len = 0 then ""
    (*If the length is not equal to zero then append the first 
    digit to the end of the remaining string reversed *)
    else String.make 1 s.[len - 1] ^ reverse_string (String.sub s 0 (len - 1))
  ;;
  
  (* Using reverse string to reverse bin *)
  let reverse_bin (m: bin) : bin =
    string_to_bin (reverse_string (bin_to_string m))
  ;;

  (* Calculating the length of bin *)
  let rec find_length (a: bin) : int = 
    match a with
    | E -> 0
    | I a' -> 1 + find_length(a')
    | O a' -> 1 + find_length(a')
  ;;

  (* Helper function to trim leading zeros from a binary number *)
  let rec trim_leading_zeros (b: bin) : bin =
    match b with
    (* An empty binary number remains empty *)
    | E -> E  
    (* Skip leading zeros until a 1 appears *)
    | O b' -> trim_leading_zeros b'  
    | I _ -> b 
;;

  (* Comparing two bin values *)
  (* The outer function reverses the bins and calls the inner function *)
  (* The inner function runs recursively to compare the two binary numbers *)
  let greater_than (m: bin) (n: bin) : bool = 
    let rec compare (m: bin) (n: bin) : bool =
      (* If the length is greter, then the number is greater too *)
      if find_length m > find_length n then true
      else if find_length m < find_length n then false
      else 
      (* When the length is equal, recursively figure out if 
        one number is greater than the other *)
      match (m, n) with
      | (E, E) -> false 
      | (E, _) -> false  
      | (_, E) -> true  
      | (I a', O b') -> true 
      | (O a', I b') -> false
      | (I a', I b') -> compare a' b' 
      | (O a', O b') -> compare a' b' 
    in
    let new_m = reverse_bin m in
    let new_n = reverse_bin n in
    compare new_m new_n
  ;;

  (* Similar to the greater than function above, with the bools reversed as necessary *)
  let lesser_than (m: bin) (n: bin) : bool = 
    let rec compare (m: bin) (n: bin) : bool =
      if find_length m > find_length n then false
      else if find_length m < find_length n then true
      else 
      match (m, n) with
      | (E, E) -> false
      | (E, _) -> true  
      | (_, E) -> false 
      | (I a', O b') -> false 
      | (O a', I b') -> true
      | (I a', I b') -> compare a' b' 
      | (O a', O b') -> compare a' b' 
    in
    let new_m = reverse_bin m in
    let new_n = reverse_bin n in
    compare new_m new_n
  ;;

  (* Checking if two binary numbers are equal *)
  let rec equal_binary (m: bin) (n: bin) : bool =
    match (m, n) with
    | (E, E) -> true
    | (I a', O b') -> false
    | (O a', I b') -> false
    | (I a', I b') -> equal_binary a' b'
    | (O a', O b') -> equal_binary a' b'
    | (E, _) -> false
    | (_, E) -> false
  ;;

  (* Multiplying binary *)
  let rec bin_mult (m: bin) (n: bin) : bin =
    match (m, n) with
    | (E, _) -> E
    | (_, E) -> E 
    | (O m', n) -> O (bin_mult m' n)
    | (n, O m') -> O (bin_mult m' n)
    | (I m', n) -> bin_add n (O (bin_mult m' n))
    | (n, I m') -> bin_add n (O (bin_mult m' n))
  ;;
  

  (* Subtracting binary *)
  let rec subtract_binary (m: bin) (n: bin) : bin =
    match (m, n) with
    | (E, E) -> E 
    | (m, E) -> m 
    (* Error if the result is negative *)
    | (E, _) -> failwith "Negative result" 
    | (I m', I n') -> O (subtract_binary m' n') 
    | (O m', O n') -> O (subtract_binary m' n')
    | (I m', O n') -> I (subtract_binary m' n')  
    (* Carry over case -> add one behind the first digit and subtract 1 from the other digit *)
    | (O m', I n') -> 
      let borrowed = subtract_binary m' (I E) in  (* Borrowing from the next bit *)
      I (subtract_binary borrowed n') (* Adding one to the difference between borriwed and n' to account 
      for the 1 in the ones positions *)
  ;;  
  
  let bool_to_string (a: bool): string =
    match a with
    | true -> "True"
    | false -> "False"
  ;;


  let ( + ) = bin_add
  let ( - ) = subtract_binary
  let ( * ) = bin_mult
  let ( > ) = greater_than
  let ( < ) = lesser_than
  let ( = ) = equal_binary
end
