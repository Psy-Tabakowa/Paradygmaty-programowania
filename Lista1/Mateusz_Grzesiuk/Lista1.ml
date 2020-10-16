(*Mateusz Grzesiuk*)

(*Zadanie 1*)

let rec flatten xs =
  if xs = [] then []
  else List.hd xs @flatten (List.tl xs);;


flatten [[1;2];[3;4];[5;6]] = [1;2;3;4;5;6];;
flatten [[5]] = [5];;

(*Zadanie 2*)

let rec count (x,xs) =
  if xs = [] then 0
  else
      if List.hd xs = x  then 1 + count(x,List.tl xs)
      else count(x,List.tl xs);;

count ('A',['A';'B';'A';'A';'C']) = 3;;
count('X', [])= 0;;

(*Zadanie 3*)

let rec replicate (x,i) =
  if i>0 then x::replicate(x,i-1)
  else [];;

replicate("la", 3) = ["la";"la";"la"];;
replicate(2, 4) = [2;2;2;2];;
replicate(1,1) = [1];;
replicate(1,0) = [];;


(*Zadanie 4*)

let rec sqrList xs =
  if(xs = []) then []
  else (List.hd xs)::(sqrList(List.tl xs));;

sqrList [1;2;3;4] == [1;4;9;16];;
sqrList [0] == [0];;
sqrList [] == [];;

(*Zadanie 5*)

let rec palindrome xs =
  if xs = [] then true
  else List.rev xs = xs;;

palindrome [1;2;1] = true;;
palindrome [1;2;3;4;5;5;4;3;2;1] = true;;
palindrome [1;2;3;4;5;4;3;2;1] = true;;
palindrome [1;2;3;4;5] = false;;
palindrome [1;2;3;4] = false;;
palindrome [1;2;3;4;2;1] = false;;
palindrome [1;2;3;4;4;3;1;1] = false;;

(*Zadanie 6*)

let rec listLength xs =
  if xs = [] then 0
  else 1 + listLength (List.tl xs);;

listLength [1;2;3] = 3;;
listLength [] = 0;;

