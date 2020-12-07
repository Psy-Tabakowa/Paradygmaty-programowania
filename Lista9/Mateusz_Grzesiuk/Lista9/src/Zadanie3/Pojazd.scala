//Mateusz Grzesiuk
package Zadanie3

//Zadanie 3

class Pojazd(private val prod: String, private val mod: String, private val ye: Int = -1, private var id: String = "") {
  def this(producent: String, model: String, id: String){
    this(producent, model, -1, id)
  }
  def producent: String = prod
  def model: String = mod
  def year: Int = ye
  def ID: String = id
  def ID_= (newID: String): Unit = ID = newID
}
