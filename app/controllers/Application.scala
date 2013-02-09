package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import play.api.libs.json._

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

  def calculateJson = Action { implicit request =>
    calculateForm.bindFromRequest.fold(
      errors => BadRequest(Json.obj("expression" -> "", "result" -> errors.toString)),
      expression => {
        val result = calculateResult(expression)
        val json = Json.obj("expression" -> expression, "result" -> result)
        Ok(json)
      }
    )
  }

  private def calculateResult(expression: String): String = {
    import calculator._

    try {
      val lexer = new Lexer(expression)
      val tokens = lexer.toTokenSeq
      val root = Parser.parse(tokens)
      val result = Evaluator.evaluate(root)
      result.toString
    } catch {
      case e => e.getMessage
    }
  }

}