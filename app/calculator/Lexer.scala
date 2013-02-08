package calculator

class ParseException(message: String) extends Exception(message)

object Tokens {
  sealed abstract trait Token
  case object LeftParen extends Token
  case object RightParen extends Token
  case class Operator(operator: Char) extends Token
  case class Number(value: String) extends Token
  case class Identifier(name: String) extends Token
}

private object TokenStream {
  val EOF = (-1).asInstanceOf[Char]
}

class TokenStream(input: String) {
  import TokenStream.EOF

  var currentChar:Int = 0

  skipWhitespace()

  private def skipWhitespace() {
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
    skipWhitespace()
    next
  }
}

object Lexer {
  val OperatorsSet = Set('+', '-', '*', '/')
}

class Lexer(input: String) {
  import Tokens._
  import scala.collection.mutable.Stack

  private val tokenStream = new TokenStream(input)
  private val tokenStack = new Stack[Token]

  isValid

  def toTokenSeq = tokenStack.toSeq.reverse

  private def number: Boolean = {
    val sb = new StringBuilder
    if (tokenStream.peek == '-') {
      sb += tokenStream.take()
    }

    if (Character.isDigit(tokenStream.peek)) {
      while (Character.isDigit(tokenStream.peek)) {
        sb += tokenStream.take()
      }

      tokenStack.push(Number(sb.toString))
      return true
    }

    false
  }

  private def identifier: Boolean = {
    if (Character.isLetter(tokenStream.peek)) {
      val sb = new StringBuilder
      while (Character.isLetter(tokenStream.peek)) {
        sb += tokenStream.take()
      }

      tokenStack.push(Identifier(sb.toString))
      return true
    }
    false
  }

  private def operator: Boolean = {
    if (Lexer.OperatorsSet(tokenStream.peek)) {
      val token = tokenStream.take()
      tokenStack.push(Operator(token))
      return true
    }
    false
  }

  private def operand: Boolean = {
    if (identifier || number) {
      return true
    }

    false
  }

  private def expression: Boolean = {
    if (tokenStream.peek == '(') {
      tokenStream.take()
      tokenStack.push(LeftParen)

      expression

      if (tokenStream.take() != ')')
        throw new ParseException("Expecting an end parenthesis")

      tokenStack.push(RightParen)
      return exprRest
    }

    if (operand) {
      return exprRest
    }

    throw new ParseException("Expecting another expression, either paren or operand.")
  }

  private def exprRest: Boolean = {
    if (operator) {
      expression
    }

    if (tokenStream.peek == TokenStream.EOF) {
      return true
    }

    false
  }

  private def isValid: Boolean =  {
    expression
  }
}

