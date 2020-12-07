//Mateusz Grzesiuk
package Zadanie1

// Zadanie 1

// a)

class Time(protected var h: Int = 0 ) {
  require(h <24, s"h=$h")
  if(h < 0) h = 0

  def hour:Int = h

  def hour_= (newh: Int): Unit ={
    require(newh <24, s"newh=$newh")
    if(newh>=0)h = newh
    else h = 0
  }

}

// b)

object Time{
  def apply(h: Int): Time = {
    var p = new Time
    p.hour = h
    p
  }
}