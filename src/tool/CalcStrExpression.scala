package tool

/**
 * Created by ckm on 2016/11/4.
 */

import scala.util.parsing.combinator.JavaTokenParsers

private[tool] abstract class Expr
private[tool] case class Variable(name: String) extends Expr
private[tool] case class Number(value: AnyVal) extends Expr
private[tool] case class UnaryOp(op: String, expr: Expr) extends Expr
private[tool] case class BinaryOp(op: String, left: Expr, right: Expr) extends Expr

object CalcStrExpression {
  object Arith extends JavaTokenParsers {
    def compose(bi: Expr, list: List[String ~ Expr]): Expr = list match {
      case Nil => bi
      case (op ~ e) :: t => compose(BinaryOp(op, bi, e), t)
    }
    def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
      case t ~ list => compose(t, list)
    }
    def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
      case t ~ list => compose(t, list)
    }
    def factor: Parser[Expr] = floatingPointNumber ^^ { x => try {Number(x.toInt)} catch {case e:Exception => Number(x.toDouble)} } | "(" ~> expr <~ ")"

    def parse(str: String): Expr = {
      val result = parseAll(expr, str)
      if (result.successful)
        result.get
      else Variable(str)
    }
  }

  def parse(str: String) = Arith.parse(str)

  def evaluate(e: Expr): Any = simplify(e) match {
    case Number(x) => x
    case Variable(x) => x
    case UnaryOp("-", x) => -evaluate(x).toString.toDouble
    case BinaryOp("+", x1, x2) => (evaluate(x1).toString.toDouble + evaluate(x2).toString.toDouble)
    case BinaryOp("-", x1, x2) => (evaluate(x1).toString.toDouble - evaluate(x2).toString.toDouble)
    case BinaryOp("*", x1, x2) => (evaluate(x1).toString.toDouble * evaluate(x2).toString.toDouble)
    case BinaryOp("/", x1, x2) => (evaluate(x1).toString.toDouble / evaluate(x2).toString.toDouble)
  }

  def evaluateStr(str: String): Any = {
    evaluate(parse(str))
  }

  /**
   * 运算符简化
   * @param e
   * @return
   */
  def simplify(e: Expr): Expr = {
    val simpSubs = e match {
      case BinaryOp(op, left, right) => BinaryOp(op, simplify(left), simplify(right))
      case UnaryOp(op, operand) => UnaryOp(op, simplify(operand))
      case _ => e
    }

    def simplifyTop(x: Expr) = x match {
      // 负负得正
      case UnaryOp("-", UnaryOp("-", x)) => x
      // 省略正号
      case UnaryOp("+", x) => x
      // 乘1为本身
      case BinaryOp("*", x, Number(1)) => x
      case BinaryOp("*", Number(1), x) => x
      // 乘0得0
      case BinaryOp("*", x, Number(0)) => Number(0)
      case BinaryOp("*", Number(0), x) => Number(0)
      // 除1为本身
      case BinaryOp("/", x, Number(1)) => x
      // 相同两个数相除为1
      case BinaryOp("/", x1, x2) if x1 == x2 => Number(1)
      // 加0为本身
      case BinaryOp("+", x, Number(0)) => x
      case BinaryOp("+", Number(0), x) => x
      case e => e
    }
    simplifyTop(simpSubs)
  }

  def main(args: Array[String]) {
    val expr = "((1 + 2) * 2 - 5 * (-1)) / 2"
    val value = evaluateStr(expr)

    println(s"Expression: $expr")
    println(s"Value: $value")

  }
}
