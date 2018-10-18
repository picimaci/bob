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
  import Identifier.PrefixOperator._
  import Ast._

  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper {
    NoTrace(" ".rep | "\t".rep)
  }

  import White.parserApi

  private val eol: P[Unit] = P("\n" | "" | "\r\n" | "\r" | "\f")

  // Values
  private val variable            = P(CharIn('a' to 'z', 'A' to 'Z', "_").rep(1))
  private val boolean: P[Boolean] = P(True | False).!.map(v => v.toBoolean)
  private val number: P[Int]      = P("-".? ~ CharIn('0' to '9').rep(1)).!.map(_.toInt)

  // Value nodes
  private val variableNode: P[VarNode] = variable.rep(1).!.map(VarNode)
  private val intLiteralExpression     = (&(number) ~ number).!.map(v => IntLiteral(v.toInt))
  private val boolLiteralExpression    = (&(boolean) ~ boolean).!.map(v => BoolLiteral(v.toBoolean))

  // Arithmetic expressions
  private val arithmeticParentheses: P[Expression] = P(
    OpeningParentheses ~ &(number) ~/ addSubExpr ~ ClosingParentheses
  )
  private val arithmeticFactor: P[Expression] = P(arithmeticParentheses | intLiteralExpression)
  private val divMulExpr: P[Expression] =
    P(arithmeticFactor ~ (CharIn(MultiplyOp + DivideOp).! ~/ arithmeticFactor).rep)
      .map(mapToExpression)
  private val addSubExpr: P[Expression] =
    P(divMulExpr ~ (CharIn(PlusOp + MinusOp).! ~/ divMulExpr).rep).map(mapToExpression)

  // Logical expressions
  private val logicalParentheses: P[Expression] = P(
    NotOp.? ~ OpeningParentheses ~ &(boolean) ~/ orExpr ~ ClosingParentheses
  )
  private val logicalFactor: P[Expression] = P(notExpr | logicalParentheses | boolLiteralExpression)
  private val andExpr: P[Expression] =
    P(logicalFactor ~ (AndOp.! ~/ logicalFactor).rep)
      .map(mapToExpression)
  private val orExpr: P[Expression] =
    P(andExpr ~ (OrOp.! ~/ andExpr).rep).map(mapToExpression)
  private val notExpr: P[Expression] =
    P(NotOp.! ~ (logicalParentheses | boolLiteralExpression)).map(v => NotExpression(v._2))
  private val boolExpr = orExpr | notExpr

  private val expression = addSubExpr | boolExpr | intLiteralExpression | boolLiteralExpression

  // Statements
  private val intVarAssignmentStatement =
    (eol.? ~ variableNode ~ AssignmentChar ~ addSubExpr ~ eol)
      .map(v => AssignmentStatement(v._1, v._2))

  private val boolVarVarAssignmentStatement =
    (eol.? ~ variableNode ~ AssignmentChar ~ boolExpr ~ eol)
      .map(v => AssignmentStatement(v._1, v._2))

  private val arithmeticStatement = (eol.? ~ addSubExpr ~ eol).map(ArithmeticStatement)
  private val boolStatement       = (eol.? ~ boolExpr ~ eol).map(BoolStatement)
  private val printStatement      = (eol.? ~ Print ~ expression ~ eol).map(PrintStatement)

  private val statements = arithmeticStatement | boolStatement | printStatement | intVarAssignmentStatement | boolVarVarAssignmentStatement

  private def mapToExpression(
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
          case AndOp      => AndExpression(left, right)
          case OrOp       => OrExpression(left, right)
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
