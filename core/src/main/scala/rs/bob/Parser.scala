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

  private val eol: P[Unit] = P("\n".rep | "".rep | "\r\n".rep | "\r".rep | "\f".rep)

  private val ws = " "

  private val keyword = (OrOp + ws) | (AndOp + ws) | (Print + ws)

  /**
    * Any identifier that contains upper cases, lower cases and underscores.
    */
  private val variable = P(!keyword ~ CharIn('a' to 'z', 'A' to 'Z', "_").rep(1))

  /**
    * Any identifier that contains literals that represent logical true and false.
    */
  private val boolean: P[Boolean] = P(True | False).!.map(v => v.toBoolean)

  /**
    * Any positive or negative number.
    */
  private val number: P[Int] = P("-".? ~ CharIn('0' to '9').rep(1)).!.map(_.toInt)

  // Value nodes
  private val variableNode: P[VariableNode] = variable.!.map(VariableNode)

  /**
    * Integer literal expression is any expression that
    * begins with the number and contains only numbers.
    */
  private val intLiteralExpression = (&(number) ~ number).!.map(v => IntLiteral(v.toInt))

  /**
    * Bool literal expression is any expression that begins with bool literals (true or false) and contains only
    * one of given bool literals.
    */
  private val boolLiteralExpression = (&(boolean) ~ boolean).!.map(v => BoolLiteral(v.toBoolean))

  /**
    * Variable literal represents a variable in an expression in the role of
    * an identifier ought to be replaced by an integer or boolean literal.
    */
  private val variableLiteral = (variableNode ~ !AssignmentChar).!.map(VariableLiteral)

  /** Arithmetic expressions parsers
    *
    * Implementation of descent recursive parser for arithmetic operations.
    * Supports multiplication, addition, subtraction and division of integers.
    */
  private val arithmeticParentheses: P[Expression] = P(
    OpeningParentheses ~ &(number) ~/ comparisonExpr ~ ClosingParentheses
  )
  private val arithmeticFactor: P[Expression] = P(
    arithmeticParentheses | intLiteralExpression | variableLiteral
  )
  private val divMulExpr: P[Expression] =
    P(arithmeticFactor ~ (CharIn(MultiplyOp + DivideOp).! ~/ arithmeticFactor).rep)
      .map(mapToExpression)
  private val addSubExpr: P[Expression] =
    P(divMulExpr ~ (CharIn(PlusOp + MinusOp).! ~/ divMulExpr).rep).map(mapToExpression)
  private val comparisonExpr = P(
    addSubExpr ~ ((NotEqualsOp | EqualsOp | GreaterThanEqualOp | LessThanEqualOp | GreaterThanOp | LessThanOp).! ~/ addSubExpr).rep
  ).map(mapToExpression)
  private val arithmeticExpr = addSubExpr

  /**
    * Logical expressions parser
    *
    * Implementation of descent recursive parser for logical operations.
    * Supports AND, OR and NOT operations.
    */
  private val logicalParentheses: P[Expression] = P(
    NotOp.? ~ OpeningParentheses ~ &(boolean) ~/ orExpr ~ ClosingParentheses
  )
  private val logicalFactor: P[Expression] = P(
    notExpr | logicalParentheses | boolLiteralExpression | comparisonExpr | variableLiteral
  )
  private val andExpr: P[Expression] =
    P(logicalFactor ~ (AndOp.! ~/ logicalFactor).rep)
      .map(mapToExpression)
  private val orExpr: P[Expression] =
    P(andExpr ~ (OrOp.! ~/ andExpr).rep).map(mapToExpression)
  private val notExpr: P[Expression] =
    P(NotOp.! ~ (logicalParentheses | boolLiteralExpression)).map(v => NotExpression(v._2))
  private val boolExpr = orExpr | notExpr

  /**
    * Represents aggregation of all other expressions.
    */
  private val expression = boolExpr | arithmeticExpr

  // Statements

  /**
    * Integer or boolean assignment statement represents any statement that
    * contains a variable identifier on the "left hand side" and an arithmetic or logical expression
    * on the "right hand side". Variables are untyped.
    *
    * Examples:
    *
    * Assignment of a simple boolean: x = true
    * Assignment of a more complex boolean expression: x = 3 + 2 > 5 and false
    * Assignment of a simple integer: x = 3
    * Assignment of a more complex integer expression: x = (2 + 2) * 5
    *
    * Assignment of an integer expression with a variable: x = (2 + 2) * z
    */
  private val varAssignmentStatement =
    (eol.? ~ variableNode ~ AssignmentChar ~ expression ~ eol)
      .map(v => AssignmentStatement(v._1, v._2))

  /**
    * Statement that can be evaluated but it's not assigned
    * to any variable.
    *
    * Examples:
    *
    * true or false
    * 5 + 2 * 3
    *
    */
  private val simpleStatement = (eol.? ~ expression ~ eol).map(SimpleStatement)

  /**
    * Represents statement ought to be printed. Contains a keyword for printing
    * and expression which is evaluated and ready for printing.
    */
  private val printStatement = (eol.? ~ (Print + ws) ~ expression ~ eol).map(PrintStatement)

  private val statements = simpleStatement | printStatement | varAssignmentStatement

  private def mapToExpression(
      tree: (Expression, Seq[(String, Expression)])
  ): Expression = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) =>
        op match {
          case PlusOp             => PlusExpression(left, right)
          case MinusOp            => MinusExpression(left, right)
          case MultiplyOp         => MultiplyExpression(left, right)
          case DivideOp           => DivideExpression(left, right)
          case AndOp              => AndExpression(left, right)
          case OrOp               => OrExpression(left, right)
          case GreaterThanOp      => GreaterThanExpression(left, right)
          case LessThanOp         => LessThanExpression(left, right)
          case EqualsOp           => EqualsExpression(left, right)
          case NotEqualsOp        => NotEqualsExpression(left, right)
          case GreaterThanEqualOp => GreaterThanEqualsExpression(left, right)
          case LessThanEqualOp    => LessThanEqualsExpression(left, right)
        }
    }
  }

  /**
    * Parses textual input into an AST.
    *
    * @param lines of the code - parses statement after the statement
    * @return AST representation of currently parsed program
    */
  def parseLines(lines: Seq[String]): Program =
    lines
      .map((s: String) => statements.parse(s))
      .map {
        case stmt: fastparse.core.Parsed.Success[Statement, _, _] => stmt.value
        case failure: fastparse.core.Parsed.Failure[_, _]         => ErrorStatement(failure.msg)
      }
      .foldLeft(Program(Vector.empty[Statement]))(
        (coll, stmt) => Program(coll.statements :+ stmt)
      )

}
