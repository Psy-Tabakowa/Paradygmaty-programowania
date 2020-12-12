//Mateusz Grzesiuk

// Zadanie 1

// a)

class Time1(protected var h: Int = 0 ) {

  require( h<24, s"h=$h")
  if(h < 0) h = 0

  def hour:Int = h

  def hour_= (newh: Int): Unit ={
    require(newh <24, s"newh=$newh")
    if(newh>=0)h = newh
    else h = 0
  }

}

// b)

object Time1{
  def apply(h: Int): Time1 = {
    val p = new Time1(h)
    p
  }
}

//Zadanie 2

// a)

class Time2A(protected var _godz:Int)(protected var _min:Int) {
  require(_godz <24, s"godz="+_godz)
  if(_godz < 0) _godz = 0
  require(_min <60, s"min="+_min)
  if(_min < 0) _min = 0

  def godz:Int = _godz
  def godz_= (newGodz: Int): Unit ={
    require(newGodz <24, s"newGodz=$newGodz")
    if(newGodz>=0) _godz = newGodz
    else _godz = 0
  }

  def min:Int = _min
  def min_= (newMin: Int): Unit ={
    require(newMin <60, s"newMin=$newMin")
    if(newMin>=0) _min = newMin
    else _min = 0
  }

  def before(other: Time2A): Boolean ={
    godz<other.godz || (godz==other.godz && min<other.min)
  }
}

// b)

class Time2B(_godz:Int)(protected var _min:Int) {

  require(_min <60, s"min="+_min)
  if(_min < 0) _min = 0
  require(_godz <24, s"godz="+_godz)
  if(_godz > 0) _min = _min+_godz*60

  def godz:Int = _min/60
  def godz_= (newGodz: Int): Unit ={
    require(newGodz <24, s"newGodz=$newGodz")
    if(newGodz>=0) _min=_min%60+newGodz*60
    else _min=_min%60
  }

  def min:Int = _min%60
  def min_= (newMin: Int): Unit ={
    require(newMin <60, s"newMin=$newMin")
    if(newMin>=0) _min = _min-_min%60+newMin
    else _min = _min-_min%60
  }

  def before(other: Time2B): Boolean ={
    godz<other.godz || (godz==other.godz && min<other.min)
  }
}

//Zadanie 3

class Pojazd(private val prod: String,
             private val mod: String,
             private val ye: Int = -1,
             private var id: String = "") {
  def this(producent: String, model: String, id: String){
    this(producent, model, -1, id)
  }
}

//Zadanie 4
object UzycieWyjatkow {
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



object Lista9 {
  def main(args: Array[String]): Unit = {
    //Zadanie 1
    val time = Time1(10)
    println(time.hour)
    //Zadanie 2 a)
    val time2 = new Time2A(10)(10)
    val time3 = new Time2A(10)(8)
    println(time3.before(time2))
    println(!time2.before(time3))
    //Zadanie 2 b)
    val time4 = new Time2B(10)(10)
    val time5 = new Time2B(10)(8)
    println(time5.before(time4))
    println(!time4.before(time5))
    println(time5.godz==10)
    println(time5.min==8)
    time5.godz = 2
    time5.min = 37
    println(time5.godz==2)
    println(time5.min==37)
    //Zadanie 3
    val pojazd = new Pojazd("BMW", "Z4")
    val pojazd2 = new Pojazd("Audi", "R8", 2019)
    val pojazd3 = new Pojazd("Ford", "Mustang", "ESI505")
    val pojazd4 = new Pojazd("Chevrolet", "Corvette", 2020, "ESI505")
    //Zadanie 4
    try {
      UzycieWyjatkow.metoda1()
    }
    catch {
      case e: Exception =>
        System.err.println(e.getMessage + "\n")
        e.printStackTrace()
    }
  }
}