package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

object Application extends Controller {

  val calculateForm = Form(
    "expression" -> nonEmptyText
  )

  def index = Action {
    Ok(views.html.index("", "", calculateForm))
  }

  def calculate = Action { implicit request =>
    calculateForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index("", "", errors)),
      expression =>
        Ok(views.html.index(calculateResult(expression), expression, calculateForm))
    )
  }

  private def calculateResult(expression: String): String = {
    import calculator._

    try {
      val lexer = new Lexer(expression)
      val tokens = lexer.toTokenSeq
      val root = Parse.parse(tokens)
      val result = Evaluate.evaluate(root)
      result.toString
    } catch {
      case e => e.getMessage
    }
  }

}