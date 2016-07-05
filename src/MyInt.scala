/**
  * Created by natalya on 04/07/2016.
  */
final case class MyInt(myInt: Int) extends MyNumber {
  def +(number: MyNumber) = number match {
    case MyInt(i) => MyInt(myInt + i)
    case MyDouble(i) => MyDouble(myInt + i)
  }

  def -(number: MyNumber) = number match {
    case MyInt(i) => MyInt(myInt - i)
    case MyDouble(i) => MyDouble(myInt - i)
  }

  def *(number: MyNumber) = number match {
    case MyInt(i) => MyInt(myInt * i)
    case MyDouble(i) => MyDouble(myInt * i)
  }

  def /(number: MyNumber) = number match {
    case MyInt(i) => MyInt(myInt / i)
    case MyDouble(i) => MyDouble(myInt / i)
  }
}
