import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class DoubleQueue[+T] private(private val head:List[T], private val tail:List[T]){

  class UnderflowException(msg: String) extends Exception(msg)

  def enqueue[S >: T](x: S):DoubleQueue[S] = {
    head match{
      case Nil => new DoubleQueue[S](x::head, tail)
      case _ => new DoubleQueue[S](head, x::tail)
    }
  }

  def dequeue:DoubleQueue[T] = {
    head match{
      case Nil => new DoubleQueue[T](Nil, Nil)
      case h::Nil => new DoubleQueue[T](tail.reverse, Nil)
      case h::t => new DoubleQueue[T](t, tail)
    }
  }

  def first:T = {
    head match {
      case Nil => throw new UnderflowException("No elements in queue")
      case h :: t => h
    }
  }

  def isEmpty:Boolean = {
    head == Nil
  }

  def equals[S >: T](obj: DoubleQueue[S]): Boolean = head == obj.head && tail == obj.tail
}
object DoubleQueue { // obiekt towarzyszący
  def empty[T] = new DoubleQueue[T](Nil, Nil)
}


object Zadanie4 {
  def copy[T](dest:collection.mutable.Seq[T], src:collection.mutable.Seq[T]): Unit ={
    var i = 0
    src.foreach(x => (dest.update(i, x), i=i+1))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    //Zadanie 3
    println(!DoubleQueue.empty.enqueue(2).isEmpty)
    println(DoubleQueue.empty.isEmpty)
    println(DoubleQueue.empty.enqueue(2).enqueue(1).dequeue.equals(DoubleQueue.empty.enqueue(2).dequeue.enqueue(1)))
    println(DoubleQueue.empty.enqueue(1).dequeue.equals(DoubleQueue.empty))
    println(DoubleQueue.empty.dequeue.equals(DoubleQueue.empty))
    println(DoubleQueue.empty.enqueue(2).enqueue(1).first == DoubleQueue.empty.enqueue(2).first)
    println(DoubleQueue.empty.enqueue(1).first == 1)
    println((try{
      DoubleQueue.empty.first
    }catch{
      case e: Exception => e.getMessage + " SAFE"
    }) == "No elements in queue SAFE")

    //Zadanie 4
    println("")
    val l2 = ListBuffer(0,0,0,0,0,0,0,0,0)

    Zadanie4.copy(l2, ListBuffer(1,2,3,4,5,6,7,8,10))
    println(l2 == ListBuffer(1,2,3,4,5,6,7,8,10))

    Zadanie4.copy(l2, ArrayBuffer(1,2,3,4,5,6,7,8,9))
    println(l2 == ListBuffer(1,2,3,4,5,6,7,8,9))

    Zadanie4.copy(ListBuffer(), ArrayBuffer())
  }
}

