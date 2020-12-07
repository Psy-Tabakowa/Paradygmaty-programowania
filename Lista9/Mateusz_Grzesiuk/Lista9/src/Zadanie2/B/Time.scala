//Mateusz Grzesiuk
package Zadanie2.B

//Zadanie 2

// b)

class Time(godz:Int, protected var min:Int = 0) {

  require(min <60, s"min=$min")
  if(min < 0) min = 0
  require(godz <24, s"godz=$godz")
  if(godz > 0) min = min+godz*60

  def hour:(Int, Int) = (min%60, min-min%60)
  def hour_= (newGodz: Int, newMin: Int): Unit ={
    require(newGodz <24, s"newh=$newGodz")
    if(newGodz>=0) min = newGodz*60
    else min = 0
    require(newMin <60, s"newm=$newMin")
    if(newMin>=0) min = min + newMin
  }

  def before(other: Time): Boolean ={
    hour._1<other.hour._1 || (hour._1==other.hour._1 && hour._2<other.hour._2)
  }
}
