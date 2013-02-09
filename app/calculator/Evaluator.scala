package calculator

import calculator.Ast.{IdentifierNode, NumberNode, Node, BinaryNode}
import calculator.Operators._

object Evaluator {
  def evaluate(root: Node): Int = root match {
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
