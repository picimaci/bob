/*
 * Copyright 2018 Branislav Lazic
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package rs.bob

import fastparse.{ WhitespaceApi, all, core }
import fastparse.all._

object Parser {
  import Keyword._
  import Identifier._
  import Identifier.InfixOperator._
  import Ast._

  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper {
    NoTrace(" ".rep | "\t".rep)
  }

  import White.parserApi

  private val eol: P[Unit] = P("\n" | "" | "\r\n" | "\r" | "\f")

  private val boolean: P[Boolean] = P(True | False).!.map(_.toBoolean)
  private val number: P[Int]      = P("-".? ~ CharIn('0' to '9').rep(1)).!.map(_.toInt)

  // Arithmetic expression parsing
  private val arithmeticParentheses: P[Int] = P(
    OpeningParentheses ~ &(number) ~/ addSub ~ ClosingParentheses
  )
  private val arithmeticFactor: P[Int] = P(arithmeticParentheses | number)
  private val divMul: P[Int] =
    P(arithmeticFactor ~ (CharIn(MultiplyOp + DivideOp).! ~/ arithmeticFactor).rep)
      .map(evalArithmeticExpression)
  private val addSub: P[Int] =
    P(divMul ~ (CharIn(AddOp + SubtractOp).! ~/ divMul).rep).map(evalArithmeticExpression)
  private val arithmeticExprNode: core.Parser[NumericExprNode, Char, String] =
    P(addSub).map(NumericExprNode)

  // Logical expression parsing
  private val logicalParentheses: P[Boolean] = P(
    OpeningParentheses ~ &(True | False) ~/ orExpr ~ ClosingParentheses
  )
  private val logicalFactor: P[Boolean] = P(logicalParentheses | boolean)
  private val andExpr: P[Boolean] =
    P(logicalFactor ~ (AndOp.! ~/ logicalFactor).rep).map(evalLogicalExpression)
  private val orExpr: P[Boolean] =
    P(andExpr ~ (OrOp.! ~/ andExpr).rep).map(evalLogicalExpression)
  private val logicalExprNode: core.Parser[BooleanExprNode, Char, String] =
    P(orExpr).map(BooleanExprNode)

  // Any string that contains lowercase, uppercase characters or underscore
  private val identNode: P[VarNode] = P(CharIn('a' to 'z', 'A' to 'Z', "_").rep(1).!.map(VarNode))

  // Parsing rules

  private val expressionNode = logicalExprNode | arithmeticExprNode

  private val exprRule = P(eol.? ~ expressionNode ~ eol)

  /*
   Assigns an expression.
   Example of literal assignment: X = 5
   Example of expression assignment: X = 3 + 6
   or: x = true and false
   */
  private val varRule = P(
    eol.? ~ identNode ~ AssignmentChar ~ expressionNode ~ eol
  )

  /*
   Prints either a variable or an expression
   Example of variable printing: print X
   Example of expression printing: print 5 + 3
   */
  private val printRule = P(eol.? ~ Print ~ (identNode | expressionNode) ~ eol)

  // Statements
  private val expressionStatement = exprRule.map(ExprStatement)
  private val assignmentStatement = varRule.map(v => AssignmentStatement(v._1, v._2))
  private val printStatement      = printRule.map(PrintStatement)
  private val parsedStatement     = P(expressionStatement | assignmentStatement | printStatement)

  private def evalArithmeticExpression(tree: (Int, Seq[(String, Int)])): Int = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) =>
        op match {
          case AddOp      => left + right
          case SubtractOp => left - right
          case MultiplyOp => left * right
          case DivideOp   => left / right
        }
    }
  }

  private def evalLogicalExpression(tree: (Boolean, Seq[(String, Boolean)])): Boolean = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) =>
        op match {
          case AndOp => left && right
          case OrOp  => left || right
        }
    }
  }

  def parseLines(lines: Seq[String]): Program =
    lines
      .map((s: String) => parsedStatement.parse(s))
      .collect {
        case stmt: fastparse.core.Parsed.Success[Statement, _, _] => stmt.value
        case failure: fastparse.core.Parsed.Failure[_, _]         => ErrorStatement(failure.toString())
      }
      .foldLeft(Program(Vector.empty[Statement]))(
        (coll, statement) => Program(coll.statements :+ statement)
      )

}
