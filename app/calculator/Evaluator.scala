package calculator

import calculator.Ast.{IdentifierNode, NumberNode, Node, BinaryNode}
import calculator.Operators._

object Evaluator {
  def evaluate(root: Node): Int = root match {
    case NumberNode(value) => value
    case IdentifierNode(name) => throw new IllegalStateException("Handling identifiers not supported yet.")
    case BinaryNode(left, right, op) => op match {
      case Multiply => evaluate(left) * evaluate(right)
      case Divide => {
        if (right == 0) {
          throw new ParseException("Cannot divide by zero")
        }

        evaluate(left) / evaluate(right)
      }
      case Add => evaluate(left) + evaluate(right)
      case Subtract => evaluate(left) - evaluate(right)
    }
  }

  def apply(input: String): String = {
    try {
      val lexer = Lexer(input)
      val tokens = lexer.toTokenSeq
      val root = Parser(tokens)
      val result = evaluate(root)
      result.toString
    } catch {
      case e => e.getMessage
    }
  }
}
