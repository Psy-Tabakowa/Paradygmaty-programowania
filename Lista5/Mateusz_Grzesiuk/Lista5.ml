(*Mateusz Grzesiuk*)

(*Zadanie 1*)

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy(lfrom (k+1)));;

let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,lazy xs)) -> x::ltake(n-1,xs)
;;


let lrepeat x ls =
  let rec lrepeatRec y ls2 =
    match ls2 with
      LNil -> LNil
    | LCons(h, lazy t) -> if y>0 then LCons(h, lazy(lrepeatRec (y-1) ls2))
                          else lrepeatRec x t
  in if x>0 then lrepeatRec x ls
     else LNil
;;

ltake (8, lrepeat 3 (lfrom 10)) = [10; 10; 10; 11; 11; 11; 12; 12];;
ltake (0, lrepeat 3 (lfrom 10)) = [];;
lrepeat 0 (lfrom 10) = LNil;;
ltake(8, lrepeat 0 (lfrom 10)) = [];;

(*Zadanie 2*)

(*a)*)

let lfib =
  let rec lfibRec x y =
    LCons(x+y, lazy(lfibRec y (x+y)))
  in LCons(0, lazy(LCons(1, lazy(lfibRec 0 1))))
;;

ltake(10, lfib) = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34];;

(*Zadanie 3*)

type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

(*a)*)

let lBreadth tree =
   let rec breadthBTRec parents queue=
    match parents with
      [] -> (match queue with
              [] -> LNil
             | _ -> breadthBTRec (List.rev queue) [])
    | h::t -> (match h with
                 LEmpty -> breadthBTRec t queue
               | LNode(x, t1, t2) -> LCons(x, lazy (breadthBTRec t (t2()::t1()::queue)))
              )
   in breadthBTRec [tree]  []
;;

(*b)*)

let rec lTree n =
  LNode(n, (function ()->lTree(2*n)), (function()->lTree(2*n+1)))
;;

ltake(10, lBreadth(lTree 1)) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;

ltake(0, lBreadth(lTree 0)) = [];;
