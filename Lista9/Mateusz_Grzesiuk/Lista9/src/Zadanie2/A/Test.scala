//Mateusz Grzesiuk

package Zadanie2.A

object Test {
  def main(args: Array[String]): Unit = {
    //Zadanie 2 a)
    val time2 = new Time(10, 10)
    val time3 = new Time(10, 8)
    println(time3.before(time2))
    println(!time2.before(time3))
  }
}
