import scala.collection.mutable.{ArrayBuffer, ListBuffer}

def copy[T](dest:collection.mutable.Seq[T], src:collection.mutable.Seq[T]): Unit ={
  src.foreach(x => dest.update(src.indexOf(x), x))
}

val l2 = ListBuffer(0,0,0,0,0,0,0,0,0)

copy(l2, ListBuffer(1,2,3,4,5,6,7,8,10))
l2 == ListBuffer(1,2,3,4,5,6,7,8,10)

copy(l2, ArrayBuffer(1,2,3,4,5,6,7,8,9))
l2 == ListBuffer(1,2,3,4,5,6,7,8,9)

copy(ListBuffer(), ArrayBuffer())



