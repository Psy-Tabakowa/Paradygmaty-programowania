//Mateusz Grzesiuk
package Zadanie2.A

//Zadanie 2

// a)

class Time(protected var godz:Int = 0, protected var min:Int = 0) {
  require(godz <24, s"godz=$godz")
  if(godz < 0) godz = 0
  require(min <60, s"min=$min")
  if(min < 0) min = 0

  def hour:(Int, Int) = (godz, min)
  def hour_= (newGodz: Int, newMin: Int): Unit ={
    require(newGodz <24, s"newh=$newGodz")
    if(newGodz>=0) godz = newGodz
    else godz = 0
    require(newMin <60, s"newm=$newMin")
    if(newMin>=0) min = newMin
    else min = 0
  }

  def before(other: Time): Boolean ={
    hour._1<other.hour._1 || (hour._1==other.hour._1 && hour._2<other.hour._2)
  }
}
