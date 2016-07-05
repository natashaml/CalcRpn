/**
  * Created by natalya on 04/07/2016.
  */
final case class MyDouble(myDouble: Double) extends MyNumber {
  def +(number: MyNumber) = number match {
    case MyDouble(d) => MyDouble(myDouble + d)
    case MyInt(d) => MyDouble(myDouble + d)
  }

  def -(number: MyNumber) = number match {
    case MyDouble(d) => MyDouble(myDouble - d)
    case MyInt(d) => MyDouble(myDouble - d)
  }

  def *(number: MyNumber) = number match {
    case MyDouble(d) => MyDouble(myDouble * d)
    case MyInt(d) => MyDouble(myDouble * d)
  }

  def /(number: MyNumber) = number match {
    case MyDouble(d) => MyDouble(myDouble / d)
    case MyInt(d) => MyDouble(myDouble / d)
  }
}