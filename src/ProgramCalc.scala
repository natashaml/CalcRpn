/**
  * Created by natalya on 05/07/2016.
  */
class ProgramCalc private(private val stack: List[MyNumber]) {
  def this() = this(Nil)

  def toMyNumber: MyNumber = this.asInstanceOf[MyNumber]

  private def opt(func: (MyNumber, MyNumber) => MyNumber): ProgramCalc = {
    stack match {
      case x :: y :: tail => new ProgramCalc(func(y, x) :: tail)
      case tail => throw new IllegalStateException("Stack is empty")
    }
  }

  def customNumber(x: MyNumber) = new ProgramCalc(stack = x :: stack)

  def + = opt(_ + _)

  def - = opt(_ - _)

  def * = opt(_ * _)

  def / = opt(_ / _)

  def result =
    if (stack.nonEmpty && stack.tail.isEmpty)
      stack.head
    else
      throw new IllegalStateException("Stack contain one element! it is not right ")
}


class RPNParser {
  private val numberRegex = """^([0-9,.]+)$""".r

  private def applyRPNPart(part: String, rpn: ProgramCalc): ProgramCalc = part match {
    case numberRegex(y) => rpn.customNumber(MyNumber(y))
    case "+" => rpn.+
    case "-" => rpn.-
    case "*" => rpn.*
    case "/" => rpn./
    case x => throw new IllegalArgumentException(x + " is't good symbol")
  }

  def calculate(expression: String): MyNumber = {
    calc(expression.split("""\s+""").toList, new ProgramCalc)
  }

  private def calc(parts: List[String], rpn: ProgramCalc): MyNumber = {
    if (parts.isEmpty) {
      rpn.result
    } else {
      try {
        calc(parts.tail, applyRPNPart(parts.head, rpn))
      } catch {
        case e@(_: IllegalStateException | _: IllegalArgumentException) =>
          throw new RuntimeException(e.getMessage() + " in: " + parts.:\("")((x, y) => x + " " + y))
      }
    }
  }
}

/** create custom Number
  *
  * @param value - Int/Double extends AnyVal).
  */
abstract class MyNumber(val value: AnyVal) {
    def +(number: MyNumber): MyNumber

    def -(number: MyNumber): MyNumber

    def *(number: MyNumber): MyNumber

    def /(number: MyNumber): MyNumber
}

  object MyNumber {

    def apply(string: String) =
      if (string.contains("."))
        MyDouble(string.toDouble)
      else
        MyInt(string.toInt)

  }

object Main {
    private def openConsoleWithCalc(console: String): Unit = {
      try {
        //if comment : var reverceString = RevercePolishNotation.reverce(console), run will be double/int also
        //and add : println(new RPNParser().calculate(console)) instead of     println(new RPNParser().calculate(reverceString.toString)),
        //example: 10.2 2 + = MyDouble(12.2)
        // but should be expression in rpn record
        var reverceString = RevercePolishNotation.reverce(console)
        println(new RPNParser().calculate(reverceString.toString))
      } catch {
        case e: RuntimeException => println(e.getMessage)
      }
    }

  def main(args: Array[String]): Unit = {
    var ok: Boolean = false
    do {
      val console = readLine("> ")

      ok = console != null && !console.isEmpty
      if (ok)
        openConsoleWithCalc(console)
    } while (ok)
  }
}