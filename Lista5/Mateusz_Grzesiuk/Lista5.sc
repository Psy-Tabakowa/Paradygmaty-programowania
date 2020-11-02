//Mateusz Grzesiuk

//Zadanie 1

def lrepeat[A](k: Int)(lxs: LazyList[A]): LazyList[A] = {
  def lrepeatRec(y: Int)(lys: LazyList[A]): LazyList[A] =
    lys match{
      case LazyList() => LazyList()
      case h#::t => if(y>0) h#::lrepeatRec(y-1)(lys) else lrepeatRec(k)(t)
    }
  if(k>0) lrepeatRec(k)(lxs)
  else LazyList()
}

lrepeat(3)(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9)).take(8).toList == List(1, 1, 1, 2, 2, 2, 3, 3)
lrepeat(0)(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9)).take(8) == LazyList()

//Zadanie 2

//b)

def lfib: LazyList[Int] = {
  def lfibRec(x: Int)(y: Int): LazyList[Int] =
    (x+y)#::lfibRec(y)(x+y)
  0#::1#::lfibRec(0)(1)
}

lfib.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

//Zadanie 3

sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem:A, left:() => lBT[A], right:() => lBT[A]) extends lBT[A]

//a)

def lBreadth [A](tree: lBT[A]): LazyList[A] = {
  def lbreadthBTRec(parents: List[lBT[A]], queue: List[lBT[A]]): LazyList[A] = {
    parents match{
      case Nil => queue match {
        case Nil => LazyList()
        case _ => lbreadthBTRec (queue.reverse, Nil)
      }
      case h :: t => h match {
        case LEmpty => lbreadthBTRec(t, queue)
        case LNode(x, t1, t2) => x#::lbreadthBTRec(t, t2() :: t1() :: queue)
      }
    }
  }
  lbreadthBTRec(List(tree), Nil)
}

//b)

def lTree(n: Int): lBT[Int] = {
  LNode[Int](n, ()=>lTree(2*n), ()=>lTree(2*n+1))
}

lBreadth(lTree(1)).take(10).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)