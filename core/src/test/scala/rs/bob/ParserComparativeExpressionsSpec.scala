package rs.bob

import org.scalatest.{ Matchers, WordSpec }
import rs.bob.Ast._
import rs.bob.Parser.parseLines

class ParserComparativeExpressionsSpec extends WordSpec with Matchers {

  "Parsing of comparative expressions" should {
    "be successful for parsing greater than expression" in {
      parseLines(Seq("1 > 2")) shouldBe Program(
        Vector(SimpleStatement(GreaterThanExpression(IntLiteral(1), IntLiteral(2))))
      )
    }

    "be successful for parsing greater than or equal expression" in {
      parseLines(Seq("1 >= 2")) shouldBe Program(
        Vector(SimpleStatement(GreaterThanEqualsExpression(IntLiteral(1), IntLiteral(2))))
      )
    }

    "be successful for parsing less than expression" in {
      parseLines(Seq("3 < 4")) shouldBe Program(
        Vector(SimpleStatement(LessThanExpression(IntLiteral(3), IntLiteral(4))))
      )
    }

    "be successful for parsing less than or equal expression" in {
      parseLines(Seq("3 <= 4")) shouldBe Program(
        Vector(SimpleStatement(LessThanEqualsExpression(IntLiteral(3), IntLiteral(4))))
      )
    }

    "be successful for parsing equals expression" in {
      parseLines(Seq("5 == 6")) shouldBe Program(
        Vector(SimpleStatement(EqualsExpression(IntLiteral(5), IntLiteral(6))))
      )
    }

    "be successful for parsing not equals expression" in {
      parseLines(Seq("5 =/= 6")) shouldBe Program(
        Vector(SimpleStatement(NotEqualsExpression(IntLiteral(5), IntLiteral(6))))
      )
    }
  }
}
