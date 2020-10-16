//Mateusz Grzesiuk

//Zadanie 2

def fib(n:Int):Int = {
  if(n==0) 0
  else if(n==1) 1
  else fib(n-1) + fib(n-2)
}

fib(1) == 1
fib(2) == 1
fib(3) == 2
fib(6) == 8
fib(10) == 55
fib(13) == 233

def fibTail(n:Int): Int = {
  def fibR(n:Int, x1:Int, x2:Int):Int = {
    if(n == 0) x1
    else fibR(n-1, x2, x1+x2)
  }
  fibR (n,0,1)
}

fibTail(1) == 1
fibTail(2) == 1
fibTail(3) == 2
fibTail(6) == 8
fibTail(10) == 55
fibTail(13) == 233

fibTail(43)//Szybszy
fib(43)


//Zadanie 3

def root3(a:Double):Double = {
  var x = a
  if(a>1) x = x/3
  def root3R(a:Double, x:Double, e:Double): Double = {
    var l=x*x*x-a
    if(l<0) l = -l
    var p=e*a
    if(p<0)p = -p
    if(l<=p) x
    else root3R(a, x+(a/x/x-x)/3, e)
  }
  root3R(a, x, 0.00000000000001)
}

root3(8) == 2
root3(-8) == -2
root3(0) == 0
root3(1) == 1
root3(729) == 9

val root3F = (a:Double) => {
  var x = 1.0
  if (a > 1) x = x / 3
  def root3R(a:Double, x:Double, e:Double): Double = {
    var l = x * x * x - a
    if (l < 0) l = -l
    var p = e * a
    if (p < 0) p = -p
    if (l <= p) x
    else root3R(a, x + (a / x / x - x) / 3, e)
  }
  root3R(a, x * a, 0.000000000000001)
}
root3F(8.0) == 2.0
root3F(-8.0) == -2.0
root3F(0.0) == 0.0
root3F(1.0) == 1.0
root3F(729.0) == 9.0

//Zadanie 4
//a)

val List(_,_,x,_,_) = List(-2,-1,0,1,2)
x == 0
//b)

val List(List(_,_),List(y,_)) = List(List(1,2), List(0,1))
y == 0

//Zadanie 5

def initSegment[A](xs:List[A],ys:List[A]): Boolean = {
  if(xs==ys) true
  else {
    def initSegmentR(xs:List[A],ys:List[A]): Boolean = {
      if(xs==Nil) true
      else if(ys==Nil) false
      else if(xs.head == ys.head) initSegmentR(xs.tail,ys.tail)
      else false
    }
    initSegmentR(xs,ys)
  }
}

initSegment(List(1,2,3,4),List(1,2,3,4,5))
initSegment(List(1),List(1,2,3,4,5))
!initSegment(List(4),List(1,2,3,4,5))
!initSegment(List(1,2,3,4,5,6),List(1,2,3,4,5))
initSegment(List(),List(1,2,3))
initSegment(List(),List())

//Zadanie 6
//a)

def replaceNth[A](xs: List[A], n: Int, x: A): List[A] = {
  if(xs.length<=n) throw new IndexOutOfBoundsException("Za mala tablica")
  else if(n<0) throw new IndexOutOfBoundsException("Ujemny indeks")
  else{
    def replaceNthR(xh: List[A],xs: List[A], n: Int, x: A): List[A] = {
      if(n==0) xh:::x::xs.tail
      else replaceNthR(xh:::List(xs.head),xs.tail, n-1, x)
    }
    replaceNthR(Nil, xs, n, x)
  }
}

replaceNth(List(1,2,3,4,5,6,7), 3, 40) == List(1,2,3,40,5,6,7)
replaceNth(List(1,2,3,4,5,6,7), 6, 70) == List(1,2,3,4,5,6,70)
replaceNth(List(1,2,3,4,5,6,7), 0, 10) == List(10,2,3,4,5,6,7)
//replaceNth(List(1,2), 5, "X")// IndexOutOfBoundsException("Za mala tablica")
//replaceNth(List(1,2), -1, "X")// IndexOutOfBoundsException("Ujemny indeks")