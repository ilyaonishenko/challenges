package hackerrank

object ReversePolishNotation extends App {

  def solve(str: String): Int = {
    val elementsInPostfix = toPostfixNotation(str)
//    println(s"postfix: $elementsInPostfix")
    executeRPN(elementsInPostfix)
  }

  object Ops {
    val ops = List("(", ")", "+", "-", "*", "/")
    val precedence = Map("-" -> 1, "+" -> 1, "/" -> 2, "*" -> 2)
    def isBracket(x: String): Boolean = isLeftBracket(x) || isRightBracket(x)
    def isOperator(x: String): Boolean = ops.contains(x) && !isBracket(x)
    def isDigit(x: String): Boolean = !ops.contains(x)
    def isLeftBracket(str: String): Boolean = str == "("
    def isRightBracket(str: String): Boolean = str == ")"
  }

  private def toPostfixNotation(str: String): List[String] = {

    val elements = str.split("\\s").toList

    val (acc, output) =
      elements.foldLeft((List.empty[String], List.empty[String])) {
        case ((acc, output), d) if Ops.isDigit(d) => (acc, output :+ d)
        case ((acc, output), op) if Ops.isOperator(op) =>
          val (head, tail) = acc.span(x => Ops.isOperator(x) && Ops.precedence(op) <= Ops.precedence(x))
          (op :: tail, output ++ head)
        case ((acc, output), s) if Ops.isLeftBracket(s) => (s :: acc, output)
        case ((acc, output), s) if Ops.isRightBracket(s) =>
          val (head, tail) = acc.span(_ != "(")
          (tail.tail, output ++ head)
      }
    output ++ acc
  }

  private def executeRPN(elementsInPostfix: List[String]): Int = {
    elementsInPostfix
      .foldLeft(List.empty[Int]) {
        case (x1 :: x2 :: xs, "+") => (x2 + x1) :: xs
        case (x1 :: x2 :: xs, "-") => (x2 - x1) :: xs
        case (x1 :: x2 :: xs, "*") => (x2 * x1) :: xs
        case (x1 :: x2 :: xs, "/") => (x2 / x1) :: xs
        case (output, token) => token.toInt :: output
      }
      .head
  }

  println(solve("3 * ( 3 - -5 )"))

}
