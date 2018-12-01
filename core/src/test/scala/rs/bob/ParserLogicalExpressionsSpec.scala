package rs.bob

import org.scalatest.{ Matchers, WordSpec }
import rs.bob.Ast._
import rs.bob.Parser.parseLines

class ParserLogicalExpressionsSpec extends WordSpec with Matchers {

  "Parsing of logical expressions" should {
    "be successful for parsing an 'OR' expression" in {
      parseLines(Seq("true or false")) shouldBe Program(
        Vector(SimpleStatement(OrExpression(BoolLiteral(true), BoolLiteral(false))))
      )
    }

    "be successful for parsing an 'AND' expression" in {
      parseLines(Seq("false and true")) shouldBe Program(
        Vector(SimpleStatement(AndExpression(BoolLiteral(false), BoolLiteral(true))))
      )
    }

    "be successful for parsing a 'NOT' expression" in {
      parseLines(Seq("! true")) shouldBe Program(
        Vector(SimpleStatement(NotExpression(BoolLiteral(true))))
      )
    }
  }
}
