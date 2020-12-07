//Mateusz Grzesiuk
package Zadanie2.B

object Test {
  def main(args: Array[String]): Unit = {
    //Zadanie 2 b)
    val time4 = new Time(10, 10)
    val time5 = new Time(10, 8)
    println(time5.before(time4))
    println(!time4.before(time5))
  }
}
