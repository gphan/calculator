package calculator

package Operators {
  sealed abstract class Op(val priority: Int) extends Ordered[Op] {
    def compare(that: Op) = this.priority compare that.priority
  }

  case object Multiply extends Op(4)
  case object Divide extends Op(3)
  case object Add extends Op(2)
  case object Subtract extends Op(1)
  case object LeftParen extends Op(0)
  case object RightParen extends Op(0)
}

package Ast {
import calculator.Operators._
sealed abstract trait AstNode
case class BinaryNode(left: AstNode, right: AstNode, operator: Op) extends AstNode
case class NumberNode(value: Int) extends AstNode
case class IdentifierNode(name: String) extends AstNode
}

object Parse {
  import Operators._
  import Ast._
  import Tokens._
  import scala.collection.mutable.Stack

  def parse(tokens: Seq[Token]): AstNode = {
    val opStack = new Stack[Op]
    val outputQueue = new Stack[AstNode]

    /**
     * Creates a binary node from two nodes in the queue.
     * @param op
     */
    def pushBinaryNode(op: Op) = {
      if (outputQueue.size < 2) {
        throw new ParseException("Could not find two nodes in the output queue.")
      }

      val right = outputQueue.pop();
      val left = outputQueue.pop();

      outputQueue.push(BinaryNode(left, right, op))
    }

    def pushToOpStack(op: Op): Unit = {
      while (!opStack.isEmpty && op < opStack.top) {
        val top = opStack.pop()
        pushBinaryNode(top)
      }

      opStack.push(op)
    }

    import Tokens._
    def processToken(token: Token): Unit = token match {
      case Number(value) => outputQueue.push(NumberNode(value.toInt))
      case Identifier(name) => outputQueue.push(IdentifierNode(name))
      case Operator(op) => op match {
        case '*' => pushToOpStack(Multiply)
        case '/' => pushToOpStack(Divide)
        case '+' => pushToOpStack(Add)
        case '-' => pushToOpStack(Subtract)
      }
      case LeftParen => opStack.push(Operators.LeftParen)
      case RightParen => {
        while (opStack.top != Operators.LeftParen) {
          val operator = opStack.pop()
          pushBinaryNode(operator)
        }
        opStack.pop()
      }
      case _ => throw new ParseException("Got an unexpected token: " + token)
    }

    tokens.foreach(processToken);

    while (!opStack.isEmpty) {
      opStack.pop() match {
        case Operators.LeftParen => throw new ParseException("Mismatched parenthesis")
        case Operators.RightParen => throw new ParseException("Mismatched parenthesis")
        case x => pushBinaryNode(x)
      }
    }

    assert(outputQueue.size == 1, "We should only have one AstNode in the output queue")
    outputQueue.pop()
  }
}

