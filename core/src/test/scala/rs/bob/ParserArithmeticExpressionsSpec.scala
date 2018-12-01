package rs.bob

import org.scalatest.{ Matchers, WordSpec }
import rs.bob.Ast._
import rs.bob.Parser.parseLines

class ParserArithmeticExpressionsSpec extends WordSpec with Matchers {

  "Parsing of arithmetic expressions" should {
    "be successful for adding two integers" in {
      parseLines(Seq("1 + 2")) shouldBe Program(
        Vector(SimpleStatement(PlusExpression(IntLiteral(1), IntLiteral(2))))
      )
    }

    "be successful for subtracting an integer from another" in {
      parseLines(Seq("3 - 4")) shouldBe Program(
        Vector(SimpleStatement(MinusExpression(IntLiteral(3), IntLiteral(4))))
      )
    }

    "be successful for multiplying two integers" in {
      parseLines(Seq("5 * 6")) shouldBe Program(
        Vector(SimpleStatement(MultiplyExpression(IntLiteral(5), IntLiteral(6))))
      )
    }

    "be successful for dividing an integer by another" in {
      parseLines(Seq("7 / 8")) shouldBe Program(
        Vector(SimpleStatement(DivideExpression(IntLiteral(7), IntLiteral(8))))
      )
    }
  }
}
