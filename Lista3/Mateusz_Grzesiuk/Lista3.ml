(*Mateusz Grzesiuk*)

(*Zadanie 2*)

let curry f x y z = f (x, y, z);;

let uncurry f (x, y, z) = f x y z;;

curry (function(x, y, z) -> x + y + z) 1 2 3 = 6;;
curry (function(x, y, z) -> 7) 0 0.0 "0" = 7;;

uncurry (function x -> function y -> function z -> x + y + z) (1,2,3) = 6;;
uncurry (function x -> function y -> function z -> 7) (0, 0.0, "0") = 7;;

(*zadanie 3*)

let sumProd xs =
  List.fold_left (function (x1, x2) -> function y -> (x1 + y, x2 * y)) (0, 1) xs;;

sumProd [1; 2; 3; 4; 5] = (15, 120);;
sumProd [] = (0, 1);;

(*Zadanie 5*)

let insertionsort f xs =
  if xs = [] then []
  else let rec insertionsortRec f xs xt xw x =
         if xs = [] || f x (List.hd xs) then
           if xw = [] then (List.rev (x :: xt)) @ xs
           else insertionsortRec f ((List.rev (x :: xt)) @ xs) [] (List.tl xw) (List.hd xw)
         else insertionsortRec f (List.tl xs) ((List.hd xs) :: xt) xw x
       in insertionsortRec f [] [] (List.tl xs) (List.hd xs)
;;


insertionsort (function x -> function y -> x < y) [1; 9; 9; 2; 6; 7; 5; 2; 6; 7; 2; 8; 52; 69; 5; 2; 6];;
insertionsort (function x -> function y -> x < y) [];;
insertionsort (function x -> function y -> x < y) [1];;

let mergesort f xs =
  let rec merge xs xt =
    if xs = [] then xt
    else if xt = [] then xs
    else if f (List.hd xs) (List.hd xt) then (List.hd xs) :: merge (List.tl xs) xt
    else (List.hd xt) :: merge xs (List.tl xt)

  in let rec mergesortRec1 xs1 xs2 =
       match xs1 with
         [] -> xs2
        |_ -> mergesortRec1 (List.tl xs1) ([List.hd xs1] :: xs2)

     in let rec mergesortRec2 xs1 xs2 =
          match (xs1, xs2) with
            ([], []) -> []
           |([], h :: []) -> h
           |([], _) -> mergesortRec2 xs2 []
           |(_, _) -> mergesortRec2
                       (match (List.tl xs1) with
                          [] -> []
                         |x -> List.tl x)
                       ((merge (List.hd xs1)(match (List.tl xs1) with
                                               [] -> []
                                              |_ -> List.hd (List.tl xs1))
                        )
                        :: xs2)
        in mergesortRec2 (mergesortRec1 xs []) []
;;

mergesort (function x -> function y -> x < y) [1; 9; 9; 2; 6; 7; 5; 2; 6; 7; 2; 8; 52; 69; 5; 2; 6];;
mergesort (function x -> function y -> x < y) [];;
mergesort (function x -> function y -> x < y) [1];;


let estimateTime f x =
  let startTime = Sys.time()
  in let fx = f x
     and estimatedTime = int_of_float((Sys.time() -. startTime) *. 1e3)
     in (fx, estimatedTime)
;;

let rec generate x xs=
  if x > 0. then generate(x -. 1.)([1; 7; 6; 5; 9; 2; 8; 6; 45; 11; 0] @ xs)
  else xs
;;

let rec testcomplexity f n=
  if n <= 0. then []
  else let (_, t)=(estimateTime (f (function x -> function y -> x < y)) (generate (2.0 ** n) []))
       in t :: testcomplexity f (n -. 1.)
;;

List.rev (testcomplexity insertionsort 10.);;
List.rev (testcomplexity mergesort 14.);;
