package rs.bob
import org.scalatest.{ Matchers, WordSpec }
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
      parseLines(Seq("""
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
      parseLines(Seq("""
          |
          | false
          |
          |
        """.stripMargin)) shouldBe Program(Vector(SimpleStatement(BoolLiteral(false))))
    }
  }
}
