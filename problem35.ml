
let is_prime n =
  let stop = sqrt (float_of_int n)
  in let rec helper k =
       if float_of_int k > stop then
         true
       else if n mod k = 0 then
         false
       else
         helper (k + 1)
     in helper 2
;;

let circles s =
  let rec looped s1 =
    let s2 = String.sub s1 1 (String.length s1 - 1) ^ String.sub s1 0 1
    in if s = s2 then
         [s1]
       else
         s1 :: looped s2
  in looped s
;;

let is_circular_prime s =
  List.for_all (fun x -> is_prime (int_of_string x)) (circles s)
;;

let count_circular_primes n =
  let rec helper m tot =
    if m = 1 then
      tot + 1 (* This function only considers odd numbers, so add 1 to account for the circular prime 2. *)
    else
      helper (m - 2) (tot + if is_circular_prime (string_of_int m) then 1 else 0)
  in helper n 0
;;

print_int (count_circular_primes 999999)
;;

print_newline ()
;;
