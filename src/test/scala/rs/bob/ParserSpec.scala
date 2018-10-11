package rs.bob
import org.scalatest.{Matchers, WordSpec}
import rs.bob.Ast.{BooleanExprNode, ExprStatement, NumericExprNode, Program}

class ParserSpec extends WordSpec with Matchers {
  import Parser._

  "Parser" should {
    "handle numeric expression" in {
      parseLines(Seq("1")) shouldBe Program(Vector(ExprStatement(NumericExprNode(1))))
    }

    "handle addition of two numbers expression" in {
      parseLines(Seq("1 + 2")) shouldBe Program(Vector(ExprStatement(NumericExprNode(3))))
    }

    "handle addition of two numbers and multiplication expression" in {
      parseLines(Seq("1 + 2 * 3")) shouldBe Program(Vector(ExprStatement(NumericExprNode(7))))
    }

    "handle addition of two numbers in parentheses and multiplication expression" in {
      parseLines(Seq("(1 + 2) * 3")) shouldBe Program(Vector(ExprStatement(NumericExprNode(9))))
    }

    """handle logical "or" operation""" in {
      parseLines(Seq("true or false")) shouldBe Program(Vector(ExprStatement(BooleanExprNode(true))))
    }

    """handle logical "and" operation""" in {
      parseLines(Seq("true and false")) shouldBe Program(Vector(ExprStatement(BooleanExprNode(false))))
    }

    """handle nested logical operations""" in {
      parseLines(Seq("(true and false) or true")) shouldBe Program(Vector(ExprStatement(BooleanExprNode(true))))
    }
  }

}
