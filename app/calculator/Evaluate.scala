package calculator

import calculator.Ast.{IdentifierNode, NumberNode, AstNode, BinaryNode}
import calculator.Operators._

object Evaluate extends App {
  val lexer = new Lexer("(3*4+5*5)/2")
  val tokens = lexer.toTokenSeq
  val root = Parse.parse(tokens)

  println(evaluate(root))

  def evaluate(root: AstNode): Int = root match {
    case NumberNode(value) => value
    case IdentifierNode(name) => throw new IllegalStateException("Handling identifiers not supported.")
    case BinaryNode(left, right, op) => op match {
      case Multiply => evaluate(left) * evaluate(right)
      case Divide => evaluate(left) / evaluate(right)
      case Add => evaluate(left) + evaluate(right)
      case Subtract => evaluate(left) - evaluate(right)
    }
  }
}
