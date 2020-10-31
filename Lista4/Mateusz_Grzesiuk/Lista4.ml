(*Mateusz Grzesiuk*)

(*Zadanie 2*)

let rec func x = func x;;

let func2 x = raise(Failure "XD");;

(*Zadanie 3*)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let breadthBT tree =
  let rec breadthBTRec parents values queue=
    match parents with
      [] -> (match queue with
              [] -> List.rev values
             | _ -> breadthBTRec (List.rev queue) values [])
    | h::t -> (match h with
                 Empty -> breadthBTRec t values queue
               | Node(x, t1, t2) -> breadthBTRec t(x::values)(t2::t1::queue))
  in breadthBTRec [tree] [] []
;;

let tt = Node(1,
              Node(2,
                   Node(4,
                        Empty,
                        Empty
                     ),
                   Empty
                ),
              Node(3,
                   Node(5,
                        Empty,
                        Node(6,
                             Empty,
                             Empty
                          )
                     ),
                   Empty
                )
           )
;;

breadthBT tt = [1; 2; 3; 4; 5; 6];;

(*Zadanie 4*)

(*a)*)

let inroute tree =
  let rec inrouteRec tree x =
    match tree with
      Empty -> 0
    | Node(_, t1, t2) -> x + inrouteRec t1 (x+1) + inrouteRec t2 (x+1)
  in inrouteRec tree 0
;;

inroute tt = 9;;

(*b)*)

let outroute tree =
  let rec outrouteRec tree x =
    match tree with
      Empty -> x
    | Node(_,t1,t2) -> outrouteRec t1 (x+1) + outrouteRec t2 (x+1)
  in outrouteRec tree 0
;;

outroute tt = 21;;

(*Zadanie 5*)

type 'a graph = Graph of ('a -> 'a list);;

 let g = Graph
(function
0 -> [3]
| 1 -> [0;2;4]
| 2 -> [1]
| 3 -> []
| 4 -> [0;2]
| n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
)
;;

let depthSearch (Graph graph) a =
  let rec check xs x =
    match xs with
      [] -> true
    | h::t -> if h = x then false
              else check t x
  in let rec depthSearchRec visited waiting =
       match waiting with
         [] -> List.rev visited
       | h::t -> if check visited h then depthSearchRec (h :: visited) ((graph h) @ t)
                 else depthSearchRec visited t
     in depthSearchRec [] [a]
;;

depthSearch g 4 = [4; 0; 3; 2; 1];;
