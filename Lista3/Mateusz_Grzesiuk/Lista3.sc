import scala.annotation.tailrec
//Mateusz Grzesiuk

//Zadanie 2

def curry3[F, A, B, C](f: (A,B,C) => F, a: A, b: B, c: C): F =
  f(a, b, c)

def uncurry3[F, A, B, C](f: A => B => C => F, a: (A, B, C)): F =
  f(a._1)(a._2)(a._3)

curry3[Int,Int,Int,Int]((x: Int, y: Int, z: Int) => x + y + z, 1, 2, 3) == 6
curry3[Int,Int,Double,String]((x: Int, y: Double, z: String) => 7, 0, 0.0, "0") == 7

uncurry3[Int,Int,Int,Int]( (x: Int) => (y: Int) => (z: Int) => x + y + z, (1, 2, 3)) == 6
uncurry3[Int,Int,Double,String]( (x: Int) => (y: Double) => (z: String) => 7, (0, 0.0, "0")) == 7

//def ultraCurry3[F,A,B,C](f: (A,B,C) => F, )



//Zadanie 3

def sumProd(xs: List[Int]): (Int, Int) =
  xs.foldLeft (0, 1)((k: (Int, Int), y: Int) => (k._1 + y, k._2 * y))

sumProd(List(1, 2, 3, 4, 5)) == (15, 120)
sumProd(Nil) == (0, 1)

//Zadanie 5

def insertionsort[A](f: A => A => Boolean, xs: List[A]): List[A] = {
  xs match{
    case Nil => Nil
    case _ =>
      @tailrec
      def insertionsortRec(f: A => A => Boolean, smaller: List[A], bigger: List[A], notCompared: List[A], x: A): List[A] =
        if (smaller == Nil || f(x)(smaller.head))
          notCompared match {
            case Nil => (x :: bigger).reverse ::: smaller
            case _ => insertionsortRec(f, (x :: bigger).reverse ::: smaller, Nil, notCompared.tail, notCompared.head)
          }
        else insertionsortRec (f, smaller.tail, smaller.head :: bigger, notCompared, x)
      insertionsortRec (f, Nil, Nil, xs.tail, xs.head)
  }
}


insertionsort ((x: Int) => (y: Int) => x < y, List(1, 9, 9, 2, 6, 7, 5, 2, 6, 7, 2, 8, 52, 69, 5, 2, 6)) == List(1, 2, 2, 2, 2, 5, 5, 6, 6, 6, 7, 7, 8, 9, 9, 52, 69)
insertionsort ((x: Int) => (y: Int) => x < y, Nil) == List()
insertionsort ((x: Int) => (y: Int) => x < y, List(1)) == List(1)


def mergesort[A](f: A => A => Boolean, xs: List[A]): List[A] = {
  def  merge (xs:List[A], ys:List[A]): List[A] =
    if (xs == Nil) ys
    else if (ys == Nil) xs
    else if (f(xs.head)(ys.head)) xs.head :: merge(xs.tail, ys)
    else ys.head :: merge(xs, ys.tail)
  @tailrec
  def mergesortRec1(toSort: List[A], sorted: List[List[A]]): List[List[A]] =
    toSort match {
      case Nil => sorted
      case _ => mergesortRec1 (toSort.tail, List(toSort.head) :: sorted)
    }
  @tailrec
  def mergesortRec2(toSort: List[List[A]], sorted: List[List[A]]): List[A] =
    (toSort, sorted) match {
      case (Nil, Nil) => Nil
      case (Nil, h :: Nil) => h
      case (Nil, _) => mergesortRec2 (sorted, Nil)
      case (_, _) => mergesortRec2 ( toSort.tail match {
        case Nil => Nil
        case x => x.tail
      },
        merge(toSort.head, toSort.tail match{
          case Nil => Nil
          case x => x.head
        }) :: sorted)
    }
  mergesortRec2 (mergesortRec1 (xs, Nil), Nil)
}

mergesort ((x: Int) => (y: Int) => x < y, List(1, 9, 9, 2, 6, 7, 5, 2, 6, 7, 2, 8, 52, 69, 5, 2, 6)) == List(1, 2, 2, 2, 2, 5, 5, 6, 6, 6, 7, 7, 8, 9, 9, 52, 69)
mergesort ((x: Int) => (y: Int) => x < y, List()) == List()
mergesort ((x: Int) => (y: Int) => x < y, List(1)) == List(1)

//Test complexity

def estimateTime[A, B](f: A => B) (x: A) = {
  val startTime = System.nanoTime
  val fx = f(x)
  val estimatedTime = (System.nanoTime - startTime) / 1000000
  (fx, estimatedTime)
}

@tailrec
def generate(x: Double, xs: List[Int]): List[Int] =
  if (x > 0.0) generate(x - 1.0, List(1, 7, 6, 5, 9, 2, 8, 6, 45, 11, 0) ::: xs)
  else xs

def testcomplexity (f: List[Int] => List[Int], n: Int): List[Long] =
  if (n <= 0.0) Nil
  else {
    val (_, t) = estimateTime [List[Int], List[Int]] (f) (generate (Math.pow(2, n), Nil))
    t :: testcomplexity (f, n - 1)
  }

val ins = (xs: List[Int]) => insertionsort[Int]((x: Int) => (y: Int) => x < y, xs)
val mer = (xs: List[Int]) => mergesort[Int]((x: Int) => (y: Int) => x < y, xs)

testcomplexity (ins, 11).reverse
testcomplexity (mer, 11).reverse
