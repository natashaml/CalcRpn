import scala.util.parsing.combinator.syntactical._

/**
  * Created by natalya on 05/07/2016.
  */
  abstract class Expression {
    def rpn:String
  }

  /**
  * @param lhs - left-associative.
  * @param rhs - right-associative.
  * @param op - binary operator.
  */
  case class Oper(lhs:Expression, op:String, rhs:Expression) extends Expression {
    def rpn:String = lhs.rpn + " " + rhs.rpn + " " + op
  }
  case class Number(v: String) extends Expression {
    def rpn:String = v
  }
  case class Variable(v: String) extends Expression {
    def rpn:String = v
  }
  case class Function(fun: String, e: List[Expression]) extends Expression {
    def rpn:String = {
    var s = ""
    e.foreach { x => s += x.rpn + " " }
    s += fun
    return s
  }
  }
  object RevercePolishNotation extends StandardTokenParsers {
    lexical.delimiters ++= List("+","-","*","/", "^","(",")",",")

    def value :Parser[Expression] = numericLit ^^ {
      s => Number(s)
    }
    def variable:Parser[Expression] =  ident ^^ {
      s => Variable(s)
    }
    def parens:Parser[Expression] = "(" ~> expr <~ ")"

    def argument:Parser[Expression] = expr <~ (","?)
    def func:Parser[Expression] = ( ident ~ "(" ~ (argument+) ~ ")" ^^ {
      case f ~ _ ~ e ~ _ => Function(f, e)
    })

    def console = (value | parens | func | variable)

    def algebraicVar :Parser[Expression] = ( console ~ "^" ~ algebraicVar ^^ {
      case left ~ _ ~ right => Oper(left, "^", right)
    }| console)
    def multiply = algebraicVar * ("*" ^^^ {
      (left:Expression, right:Expression) => Oper(left, "*", right)
    } | "/" ^^^ { (left:Expression, right:Expression) => Oper(left, "/", right)
    } )
    def sum =  multiply * ("+" ^^^ {
      (left:Expression, right:Expression) => Oper(left, "+", right)
    } | "-" ^^^ { (left:Expression, right:Expression) => Oper(left, "-", right)
    } )

    def expr = ( sum | console )

    def parseStr(s:String) = {
      val tokens = new lexical.Scanner(s)
        phrase(expr)(tokens)
    }

    def reverce(exprstr: String) : String = exprstr match {
      case null => return ""
      case "" => return ""
      case _ =>
        parseStr(exprstr) match {
          case Success(tree, _) =>
            val v = tree.rpn
            return v
          case e: NoSuccess => Console.err.println(e)
            return e.toString
        }
    }
}


