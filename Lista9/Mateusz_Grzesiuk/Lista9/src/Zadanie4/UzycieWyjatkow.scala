//Mateusz Grzesiuk
package Zadanie4

//Zadanie 4

object UzycieWyjatkow {
  def main(args: Array[String]): Unit = {
    try metoda1()
    catch {
      case e: Exception =>
        System.err.println(e.getMessage + "\n")
        e.printStackTrace()
    }
  }

  @throws[Exception]
  def metoda1(): Unit = {
    metoda2()
  }

  @throws[Exception]
  def metoda2(): Unit = {
    metoda3()
  }

  @throws[Exception]
  def metoda3(): Unit = {
    throw new Exception("Wyjatek zgloszony w metoda3")
  }
}
