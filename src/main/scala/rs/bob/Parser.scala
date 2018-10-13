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

import fastparse.WhitespaceApi
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

  // Values
  private val variable            = P(CharIn('a' to 'z', 'A' to 'Z', "_").rep(1))
  private val boolean: P[Boolean] = P(True | False).!.map(_.toBoolean)
  private val number: P[Int]      = P("-".? ~ CharIn('0' to '9').rep(1)).!.map(_.toInt)

  private val operand = (variable | boolean | number).!.map(OperandNode)

  // Value nodes
  private val variableNode: P[VarNode] = variable.rep(1).!.map(VarNode)
  private val intLiteralExpression     = (&(number) ~ operand).!.map(v => IntLiteral(v.toInt))
  private val boolLiteralExpression    = (&(boolean) ~ operand).!.map(v => BoolLiteral(v.toBoolean))

  // Arithmetic expressions
  private val arithmeticParentheses: P[Expression] = P(
    OpeningParentheses ~ &(number) ~/ addSub ~ ClosingParentheses
  )
  private val arithmeticFactor: P[Expression] = P(arithmeticParentheses | intLiteralExpression)
  private val divMul: P[Expression] =
    P(arithmeticFactor ~ (CharIn(MultiplyOp + DivideOp).! ~/ arithmeticFactor).rep)
      .map(evalArithmeticExpression)
  private val addSub: P[Expression] =
    P(divMul ~ (CharIn(PlusOp + MinusOp).! ~/ divMul).rep).map(evalArithmeticExpression)

  // Logical expressions
  private val orExpression =
    (boolLiteralExpression ~ OrOp ~ boolLiteralExpression).map(v => BoolExpression(v._1, v._2))

  private val expression = addSub | intLiteralExpression | boolLiteralExpression

  // Statements
  private val intVarAssignmentStatement =
    (eol.? ~ variableNode ~ AssignmentChar ~ addSub ~ eol)
      .map(v => AssignmentStatement(v._1, v._2))

  private val boolVarVarAssignmentStatement =
    (eol.? ~ variableNode ~ AssignmentChar ~ (orExpression | boolLiteralExpression) ~ eol)
      .map(v => AssignmentStatement(v._1, v._2))

  private val arithmeticStatement = (eol.? ~ addSub ~ eol).map(ArithmeticStatement)
  private val printStatement      = (eol.? ~ Print ~ expression ~ eol).map(PrintStatement)

  private val statements = arithmeticStatement | printStatement | intVarAssignmentStatement | boolVarVarAssignmentStatement

  private def evalArithmeticExpression(
      tree: (Expression, Seq[(String, Expression)])
  ): Expression = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) =>
        op match {
          case PlusOp     => PlusExpression(left, right)
          case MinusOp    => MinusExpression(left, right)
          case MultiplyOp => MultiplyExpression(left, right)
          case DivideOp   => DivideExpression(left, right)
        }
    }
  }

  def parseLines(lines: Seq[String]): Program =
    lines
      .map((s: String) => statements.parse(s))
      .collect {
        case stmt: fastparse.core.Parsed.Success[Statement, _, _] => stmt.value
        case failure: fastparse.core.Parsed.Failure[_, _]         => ErrorStatement(failure.toString())
      }
      .foldLeft(Program(Vector.empty[Statement]))(
        (coll, stmt) => Program(coll.statements :+ stmt)
      )

}
