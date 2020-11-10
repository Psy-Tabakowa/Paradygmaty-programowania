import scala.Console.println
import scala.annotation.tailrec
//Mateusz Grzesiuk

//Zadanie 1

@tailrec
def whileLoop(f: => Boolean)(w: => Unit): Unit =
  if(f){
    w
    whileLoop(f)(w)
  }

var x=7
var l = List[Int]()
whileLoop(x>0)({l = x::l;x=x-1})
l == List(1, 2, 3, 4, 5, 6, 7)


//Zadanie 2

//a)

def swap(tab: Array[Int])(i: Int)(j: Int): Unit ={
  x = tab(i)
  tab(i) = tab(j)
  tab(j) = x
}

var array1 = Array(1,2,3,4,5)
swap(array1)(2)(4)
array1 sameElements Array(1, 2, 5, 4, 3)

//b)

def partition(tab: Array[Int])(l: Int)(r: Int): (Int, Int) = {
  var i = l
  var j = r
  val pivot = tab((i+j)/2)
  while (i<=j) {
    while (tab(i) < pivot) i+=1
    while (tab(j) > pivot) j-=1
    if (i <= j) {
      swap(tab)(i)(j)
      i+=1
      j-=1
    }
  }
  (i, j)
}

//c)

def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
  if(l<r) {
    val (i, j) = partition(tab)(l)(r)
    quick(tab)(l)(j)
    quick(tab)(i)(r)
  }

//d)

def quicksort(tab: Array[Int]): Unit =
  quick (tab)(0)(tab.length-1)

var array2 = Array(9,6,8,52,3,0,4,-1,76,9,1,5,2,12,2,2,2)
quicksort(array2)
array2 sameElements Array(-1, 0, 1, 2, 2, 2, 2, 3, 4, 5, 6, 8, 9, 9, 12, 52, 76)

var array3 = Array[Int]()
quicksort(array3)
array3 sameElements Array[Int]()

var array4 = Array(1)
quicksort(array4)
array4 sameElements Array(1)