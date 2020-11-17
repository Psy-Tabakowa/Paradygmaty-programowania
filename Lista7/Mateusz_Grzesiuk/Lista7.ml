(*Mateusz Grzesiuk*)

(*Zadanie 1*)

module type QUEUE_FUN =
sig
 type 'a t
 exception Empty of string
 val empty: unit -> 'a t
 val enqueue: 'a * 'a t -> 'a t
 val dequeue: 'a t -> 'a t
 val first: 'a t -> 'a
 val isEmpty: 'a t -> bool
end;;

(*a)*)

module QUEUE_ONE: QUEUE_FUN =
  struct
    type 'a t = EmptyQueue | Queue of 'a list
    exception Empty of string
    let empty() = EmptyQueue
    let enqueue(x, q) =
      match q with
        Queue(xl) -> Queue(x::xl)
      | EmptyQueue -> Queue([x])
    let rec dequeue q =
      match q with
        EmptyQueue -> EmptyQueue
      | Queue([])  -> EmptyQueue
      | Queue(_::[]) -> EmptyQueue
      | Queue(h::t) -> enqueue(h, dequeue (Queue(t)))
    let rec first xl =
      match xl with
        EmptyQueue -> raise (Empty "No elements in queue")
      | Queue([]) -> raise (Empty "No elements in queue")
      | Queue(h::[]) -> h   
      | Queue(_::t) -> first (Queue(t))
    let isEmpty xl =
      match xl with
        EmptyQueue -> true
      | Queue([]) -> true
      | _ -> false
  end;;

let queue1 = QUEUE_ONE.empty();;
(*QUEUE_ONE.first(queue1);;*)(*Raises failure 'Empty'*)
QUEUE_ONE.isEmpty queue1;;
let queue2 = QUEUE_ONE.enqueue(6, QUEUE_ONE.enqueue(5, QUEUE_ONE.enqueue(4, QUEUE_ONE.enqueue(3, QUEUE_ONE.enqueue(2,QUEUE_ONE.enqueue(1, queue1))))));;
QUEUE_ONE.first queue2 = 1;;
QUEUE_ONE.isEmpty queue2 = false;;
let queue3 = (QUEUE_ONE.dequeue (QUEUE_ONE.dequeue (QUEUE_ONE.dequeue queue2)));;
QUEUE_ONE.first queue3 = 4;;
let queue4 = (QUEUE_ONE.dequeue (QUEUE_ONE.dequeue (QUEUE_ONE.dequeue queue3)));;
(*QUEUE_ONE.first(queue4);;*)(*Raises failure 'Empty'*)
QUEUE_ONE.isEmpty queue4;;
QUEUE_ONE.isEmpty (QUEUE_ONE.dequeue(queue4));;


(*b)*)

module QUEUE_TWO: QUEUE_FUN =
  struct
    type 'a t = EmptyQueue | Queue of 'a list*'a list
    exception Empty of string
    let empty() = EmptyQueue
    let enqueue(x, q) =
      match q with
        Queue(xl1, xl2) -> Queue(xl1,x::xl2)
      | EmptyQueue -> Queue([],[x])
    let rec dequeue q =
      match q with
        EmptyQueue -> EmptyQueue
      | Queue([],[])  -> EmptyQueue
      | Queue([],xl2) -> dequeue (Queue((List.rev xl2), []))
      | Queue(h::t,xl2) -> Queue(t, xl2)
    let rec first xl =
      match xl with
        EmptyQueue -> raise (Empty "No elements in queue")
      | Queue([], []) -> raise (Empty "No elements in queue")
      | Queue(h::_, _) -> h   
      | Queue([], xl2) -> first (Queue((List.rev xl2), []))
    let isEmpty xl =
      match xl with
        EmptyQueue -> true
      | Queue([], []) -> true
      | _ -> false
  end;;

let queue1 = QUEUE_TWO.empty();;
(*QUEUE_ONE.first(queue1);;*)(*Raises failure 'Empty'*)
QUEUE_TWO.isEmpty queue1;;
let queue2 = QUEUE_TWO.enqueue(6, QUEUE_TWO.enqueue(5, QUEUE_TWO.enqueue(4, QUEUE_TWO.enqueue(3, QUEUE_TWO.enqueue(2, QUEUE_TWO.enqueue(1, queue1))))));;
QUEUE_TWO.first queue2 = 1;;
QUEUE_TWO.isEmpty queue2 = false;;
let queue3 = (QUEUE_TWO.dequeue (QUEUE_TWO.dequeue (QUEUE_TWO.dequeue queue2)));;
QUEUE_TWO.first queue3 = 4;;
let queue4 = (QUEUE_TWO.dequeue (QUEUE_TWO.dequeue (QUEUE_TWO.dequeue queue3)));;
(*QUEUE_ONE.first(queue4);;*)(*Raises failure 'Empty'*)
QUEUE_TWO.isEmpty queue4;;
QUEUE_TWO.isEmpty (QUEUE_TWO.dequeue(queue4));;


(*Zadanie 2*)

module type QUEUE_MUT =
sig
 type 'a t
 (* The type of queues containing elements of type ['a]. *)
 exception Empty of string
 (* Raised when [first q] is applied to an empty queue [q]. *)
 exception Full of string
 (* Raised when [enqueue(x,q)] is applied to a full queue [q]. *)
 val empty: int -> 'a t
 (* [empty n] returns a new queue of length [n], initially empty. *)
 val enqueue: 'a * 'a t -> unit
 (* [enqueue (x,q)] adds the element [x] at the end of a queue [q]. *)
 val dequeue: 'a t -> unit
 (* [dequeue q] removes the first element in queue [q] *)
 val first: 'a t -> 'a
 (* [first q] returns the first element in queue [q] without removing
 it from the queue, or raises [Empty] if the queue is empty. *)
 val isEmpty: 'a t -> bool
 (* [isEmpty q] returns [true] if queue [q] is empty,
 otherwise returns [false]. *)
 val isFull: 'a t -> bool
 (* [isFull q] returns [true] if queue [q] is full,
 otherwise returns [false]. *)
end;;

module QUEUE_CYCL: QUEUE_MUT =
  struct
    type 'b field = EmptyField | Field of 'b
    type 'a t = Queue of 'a field array*int ref*int ref
    exception Empty of string
    exception Full of string
    let empty n = Queue((Array.make n EmptyField), (ref 0), (ref 0))
    let enqueue (x, q) =
      match q with
        Queue(array, start, finish) -> match array.(!finish) with
                                         Field(_) -> raise (Full "Queue is full")
                                       | EmptyField -> let _ = array.(!finish) <- (Field x)
                                                       in finish:=(!finish+1) mod (Array.length array)
    let dequeue q =
      match q with
        Queue(array, start, finish) -> match array.(!start) with
                                         Field(_) -> let _ = array.(!start) <- EmptyField
                                                     in start:=(!start+1) mod (Array.length array)
                                       | EmptyField -> ()
    let first q =
      match q with
        Queue(array, start, finish) -> match array.(!start) with
                                         EmptyField -> raise (Empty "No elements in queue")
                                       | Field(x) -> x
    let isEmpty q =
      match q with
        Queue(array, start, finish) -> array.(!start) = EmptyField
    let isFull q =
      match q with
        Queue(array, start, finish) -> !start = !finish
  end;;

let queue1 = QUEUE_CYCL.empty 6;;
(*QUEUE_ONE.first(queue1);;*)(*Raises failure 'Empty'*)
QUEUE_CYCL.isEmpty queue1;;
QUEUE_CYCL.enqueue(1, queue1);;
QUEUE_CYCL.enqueue(2, queue1);;
QUEUE_CYCL.enqueue(3, queue1);;
QUEUE_CYCL.isFull queue1 = false;;
QUEUE_CYCL.first queue1 = 1;;
QUEUE_CYCL.isEmpty queue1 = false;;
QUEUE_CYCL.dequeue queue1;;
QUEUE_CYCL.dequeue queue1;;
QUEUE_CYCL.first queue1 = 3;;
QUEUE_CYCL.dequeue queue1;;
QUEUE_CYCL.dequeue queue1;;
QUEUE_CYCL.enqueue(4, queue1);;
QUEUE_CYCL.enqueue(5, queue1);;
QUEUE_CYCL.enqueue(6, queue1);;
QUEUE_CYCL.enqueue(7, queue1);;
QUEUE_CYCL.enqueue(8, queue1);;
QUEUE_CYCL.enqueue(9, queue1);;
QUEUE_CYCL.isFull queue1;;
(*QUEUE_CYCL.enqueue(10, queue1);;*)(*Raises failure 'Full'*)
QUEUE_CYCL.first queue1 = 4;;
QUEUE_CYCL.dequeue queue1;;
QUEUE_CYCL.enqueue(10, queue1);;
QUEUE_CYCL.first queue1 = 5;;
QUEUE_CYCL.isFull queue1;;
