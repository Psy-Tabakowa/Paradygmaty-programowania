(*Mateusz Grzesiuk*)

(*Zadanie 2*)

let rec fib n =
  if n=0 then 0
  else if n=1 then 1
  else fib(n-1)+fib(n-2);;

let fibTail n =
  let rec fibR (n, x1, x2) =
    if n=0 then x1
    else fibR (n-1, x2, x1+x2)
  in fibR(n, 0, 1);;

fib 1 = 1;;
fib 2 = 1;;
fib 3 = 2;;
fib 6 = 8;;
fib 10 = 55;;
fib 13 = 233;;

fibTail 1 = 1;;
fibTail 2 = 1;;
fibTail 3 = 2;;
fibTail 6 = 8;;
fibTail 10 = 55;;
fibTail 13 = 233;;

fibTail 43;;//Szybszy
(*fib 43;;*)

(*Zadanie 3*)

let root3 a =
  let x = if a>1.0 then a/.3. else a in
  let rec root3R(a, x, e) =
    let l = if x*.x*.x-.a<0.0 then a-.x*.x*.x else x*.x*.x-.a
    and p = if e*.a<0.0 then -.e*.a else e*.a in
        if l<=p then x
        else root3R(a, x+.(a/.x/.x-.x)/.3., e)
  in root3R(a, x, 0.000000000000001);;

root3 8.0 = 2.;;
root3 (-8.0) = -2.;;
root3 0.0 = 0.;;
root3 1.0 = 1.;;
root3 729.0 = 9.;;

(*Zadanie 4*)
(*a)*)
let (_,_,x,_,_) = (-2,-1,0,1,2);;
x=0;;

(*b)*)
let ((_,_),(y,_)) = ((1,2),(0,1));;
y=0;;



(*Zadanie 5*)

let initSegment (xs, ys) =
  if xs=ys then true
  else
    let rec initSegmentR(xs, ys) =
      if xs = [] then true
      else if ys =[] then false
      else if List.hd xs = List.hd ys then initSegmentR(List.tl xs, List.tl ys)
      else false
    in initSegmentR(xs, ys);;

initSegment ([1;2;3;4],[1;2;3;4;5]);;
initSegment ([1],[1;2;3;4;5]);;
initSegment ([4],[1;2;3;4;5]) = false;;
initSegment ([1;2;3;4;5;6],[1;2;3;4;5]) = false;;
initSegment ([],[1;2;3]);;
initSegment ([],[]);;

(*Zadanie 6*)
(*a)*)

let replaceNth(xs, n, x) =
  if List.length xs < n then raise (Failure "Za mala tablica")
  else if n<0 then raise (Failure"Ujemny indeks")
  else
    let rec replaceNthR(xh, xs, n, x) =
      if n=0 then xh@x::List.tl xs
      else replaceNthR(xh@[List.hd xs], List.tl xs, n-1, x)
    in replaceNthR([], xs, n, x);;

replaceNth ([1;2;3;4;5;6;7], 3, 40) = [1;2;3;40;5;6;7];;
replaceNth ([1;2;3;4;5;6;7], 6, 70) = [1;2;3;4;5;6;70];;
replaceNth ([1;2;3;4;5;6;7], 0, 10) = [10;2;3;4;5;6;7];;
(*replaceNth([1;2], 5, "X")*) (*IndexOutOfBoundsException("Za mala tablica")*)
(*replaceNth([1;2], -1, "X")*) (*IndexOutOfBoundsException("Ujemny indeks")*)
