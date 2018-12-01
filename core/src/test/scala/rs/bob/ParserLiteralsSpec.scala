package rs.bob
import org.scalatest.{Matchers, WordSpec}
import rs.bob.Ast._

class ParserLiteralsSpec extends WordSpec with Matchers {
  import Parser._

  "Parsing of literals" should {
    "be successful for simple integer" in {
      parseLines(Seq("5")) shouldBe Program(Vector(SimpleStatement(IntLiteral(5))))
    }

    "be successful for simple integer prefixed and suffixed with spaces" in {
      parseLines(Seq("    5       ")) shouldBe Program(Vector(SimpleStatement(IntLiteral(5))))
    }

    "be successful for simple integer with break lines" in {
      parseLines(Seq(
        """
          |  5
          |
        """.stripMargin)) shouldBe Program(Vector(SimpleStatement(IntLiteral(5))))
    }

    "be successful for simple boolean" in {
      parseLines(Seq("true")) shouldBe Program(Vector(SimpleStatement(BoolLiteral(true))))
    }

    "be successful for simple boolean prefixed and suffixed with spaces" in {
      parseLines(Seq("      true   ")) shouldBe Program(Vector(SimpleStatement(BoolLiteral(true))))
    }

    "be successful for simple boolean with break lines" in {
      parseLines(Seq(
        """
          |
          | false
          |
          |
        """.stripMargin)) shouldBe Program(Vector(SimpleStatement(BoolLiteral(false))))
    }
  }

  "Parsing of arithmetic expressions" should {
    "be successful for adding two integers" in {
      parseLines(Seq("1 + 2")) shouldBe Program(Vector(SimpleStatement(PlusExpression(IntLiteral(1),IntLiteral(2)))))
    }

    "be successful for subtracting an integer from another" in {
      parseLines(Seq("3 - 4")) shouldBe Program(Vector(SimpleStatement(MinusExpression(IntLiteral(3),IntLiteral(4)))))
    }

    "be successful for multiplying two integers" in {
      parseLines(Seq("5 * 6")) shouldBe Program(Vector(SimpleStatement(MultiplyExpression(IntLiteral(5),IntLiteral(6)))))
    }

    "be successful for dividing an integer by another" in {
      parseLines(Seq("7 / 8")) shouldBe Program(Vector(SimpleStatement(DivideExpression(IntLiteral(7),IntLiteral(8)))))
    }
  }

  "Parsing of logical expressions" should {
    "be successful for parsing an 'OR' expression" in {
      parseLines(Seq("true or false")) shouldBe Program(Vector(SimpleStatement(OrExpression(BoolLiteral(true), BoolLiteral(false)))))
    }

    "be successful for parsing an 'AND' expression" in {
      parseLines(Seq("false and true")) shouldBe Program(Vector(SimpleStatement(AndExpression(BoolLiteral(false), BoolLiteral(true)))))
    }

    "be successful for parsing a 'NOT' expression" in {
      parseLines(Seq("! true")) shouldBe Program(Vector(SimpleStatement(NotExpression(BoolLiteral(true)))))
    }
  }

  "Parsing of comparative expressions" should {
    "be successful for parsing greater than expression" in {
      parseLines(Seq("1 > 2")) shouldBe Program(Vector(SimpleStatement(GreaterThanExpression(IntLiteral(1),IntLiteral(2)))))
    }

    "be successful for parsing greater than or equal expression" in {
      parseLines(Seq("1 >= 2")) shouldBe Program(Vector(SimpleStatement(GreaterThanEqualsExpression(IntLiteral(1),IntLiteral(2)))))
    }

    "be successful for parsing less than expression" in {
      parseLines(Seq("3 < 4")) shouldBe Program(Vector(SimpleStatement(LessThanExpression(IntLiteral(3),IntLiteral(4)))))
    }

    "be successful for parsing less than or equal expression" in {
      parseLines(Seq("3 <= 4")) shouldBe Program(Vector(SimpleStatement(LessThanEqualsExpression(IntLiteral(3),IntLiteral(4)))))
    }

    "be successful for parsing equals expression" in {
      parseLines(Seq("5 == 6")) shouldBe Program(Vector(SimpleStatement(EqualsExpression(IntLiteral(5),IntLiteral(6)))))
    }

    "be successful for parsing not equals expression" in {
      parseLines(Seq("5 =/= 6")) shouldBe Program(Vector(SimpleStatement(NotEqualsExpression(IntLiteral(5),IntLiteral(6)))))
    }
  }
}
