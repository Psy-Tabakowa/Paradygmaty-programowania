import scala.annotation.tailrec
//Mateusz Grzesiuk

//Zadanie 3

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

def breadthBT [A](tree: BT[A]): List[A] = {
  @tailrec
  def breadthBTRec(parents: List[BT[A]], values: List[A], queue: List[BT[A]]): List[A] = {
    parents match{
      case Nil => queue match {
        case Nil => values.reverse
        case _ => breadthBTRec (queue.reverse, values, Nil)
      }
      case h :: t => h match {
        case Empty => breadthBTRec(t, values, queue)
        case Node(x, t1, t2) => breadthBTRec(t, x :: values, t2 :: t1 :: queue)
      }
    }
  }
  breadthBTRec(List(tree), Nil, Nil)
}

val t = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))

breadthBT(t) == List(1, 2, 3, 4, 5, 6)

//Zadanie 4

//a)

def inroute[A](tree: BT[A]): Int = {
  def inrouteRec(tree: BT[A], x: Int): Int = {
    tree match {
      case Empty => 0
      case Node(_, t1, t2) => x + inrouteRec(t1, x + 1) + inrouteRec(t2, x + 1)
    }
  }
  inrouteRec(tree, 0)
}

inroute(t) == 9

//b)

def outroute[A](tree: BT[A]): Int = {
  def outrouteRec(tree: BT[A], x: Int): Int = {
    tree match {
      case Empty => x
      case Node(_, t1, t2) => outrouteRec(t1, x + 1) + outrouteRec(t2, x + 1)
    }
  }
  outrouteRec(tree, 0)
}

outroute(t) == 21

//Zadanie 5

sealed trait Graphs[A]
case class Graph[A](succ: A => List[A]) extends Graphs[A]

val g = Graph((i: Int) => i match {
  case 0 => List(3)
  case 1 => List(0, 2, 4)
  case 2 => List(1)
  case 3 => Nil
  case 4 => List(0, 2)
  case n => throw new Exception("Graph g: node " + n
    + " doesn't exist")
})

def depthSearch[A](graph: Graph[A], a: A): List[A] = {
  @tailrec
  def check(xs: List[A], x: A): Boolean =
    xs match {
      case Nil => true
      case h :: t => if(h == x) false
      else check(t, x)
    }
  @tailrec
  def depthSearchRec(visited: List[A], waiting: List[A]): List[A] =
    waiting match {
      case Nil => visited.reverse
      case h :: t => if(check(visited, h)) depthSearchRec (h :: visited, (graph succ h) ::: t)
      else depthSearchRec(visited, t)
    }
  depthSearchRec(Nil, List(a))
}

depthSearch(g, 4) == List(4, 0, 3, 2, 1)