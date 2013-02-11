package calculator

class ParseException(message: String) extends Exception(message)

object Token {
  sealed trait Token
  case object LeftParenthesisToken extends Token
  case object RightParenthesisToken extends Token
  case class OperatorToken(operator: Char) extends Token
  case class NumberToken(value: String) extends Token
  case class IdentifierToken(name: String) extends Token
}

private object TokenStream {
  val EOF = (-1).asInstanceOf[Char]
}

class TokenStream(input: String) {
  import TokenStream.EOF

  var currentChar = 0

  skipWhitespace()

  def skipWhitespace() {
    while (currentChar < input.length && input(currentChar) == ' ') { currentChar += 1 }
  }

  def peek: Char = {
    if (currentChar >= input.length)
      return EOF

    input(currentChar)
  }

  def take(): Char = {
    if (currentChar >= input.length)
      return EOF

    val next = input(currentChar)
    currentChar += 1
    next
  }
}

object Lexer {
  val OperatorsSet = Set('+', '-', '*', '/')

  def apply(input: String) = new Lexer(input)
}

class Lexer(input: String) {
  import Token._
  import scala.collection.mutable.Stack

  private val tokenStream = new TokenStream(input)
  private val tokenStack = new Stack[Token]

  // Run the expression to validate it
  expression()

  def toTokenSeq = tokenStack.toSeq.reverse

  private def number(): Boolean = {
    val sb = new StringBuilder
    if (tokenStream.peek == '-') {
      sb += tokenStream.take()
    }

    if (Character.isDigit(tokenStream.peek)) {
      while (Character.isDigit(tokenStream.peek)) {
        sb += tokenStream.take()
      }

      tokenStack.push(NumberToken(sb.toString))
      return true
    }

    false
  }

  private def identifier(): Boolean = {
    if (Character.isLetter(tokenStream.peek)) {
      val sb = new StringBuilder
      while (Character.isLetter(tokenStream.peek)) {
        sb += tokenStream.take()
      }

      tokenStack.push(IdentifierToken(sb.toString))
      return true
    }
    false
  }

  private def operator(): Boolean = {
    if (Lexer.OperatorsSet(tokenStream.peek)) {
      val token = tokenStream.take()
      tokenStack.push(OperatorToken(token))
      return true
    }
    false
  }

  private def operand(): Boolean = {
    if (identifier() || number()) {
      return true
    }

    false
  }

  private def expression(): Boolean = {
    tokenStream.skipWhitespace()
    if (tokenStream.peek == '(') {
      tokenStream.take()
      tokenStack.push(LeftParenthesisToken)

      expression()

      if (tokenStream.take() != ')')
        throw new ParseException("Expecting an end parenthesis")

      tokenStack.push(RightParenthesisToken)
      return exprRest()
    }

    if (operand()) {
      return exprRest()
    }

    throw new ParseException("Expecting another expression, either parenthesis or operand.")
  }

  private def exprRest(): Boolean = {
    tokenStream.skipWhitespace()
    if (operator()) {
      return expression()
    }

    if (tokenStream.peek == TokenStream.EOF) {
      return true
    }

    if (operand()) {
      throw new ParseException("Unexpected operand")
    }

    return false
  }
}

