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

import fastparse.{ WhitespaceApi, core }
import fastparse.all._

object Parser {
  import Keyword._
  import Identifier._
  import Ast._

  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper {
    NoTrace(" ".rep)
  }

  import White.parserApi

  private val eol: P[Unit] = P("\n" | " " | "")

  private val number: P[Int] = P(CharIn('0' to '9').rep(1).!.map(_.toInt))

  // Expression parsing
  private val parentheses: P[Int] = P(OpeningParentheses ~/ addSub ~ ClosingParentheses)
  private val factor: P[Int]      = P(number | parentheses)
  private val divMul: P[Int] =
    P(factor ~ (CharIn(MultiplyChar + DivideChar).! ~/ factor).rep).map(evalExpression)
  private val addSub: P[Int] =
    P(divMul ~ (CharIn(AddChar + SubtractChar).! ~/ divMul).rep).map(evalExpression)
  private val expr: core.Parser[ExprNode, Char, String] = P(addSub).map(ExprNode)

  // Any string that contains lowercase, uppercase characters or underscore
  private val ident: P[VarNode] = P(CharIn('a' to 'z', 'A' to 'Z', "_").rep(1).!.map(VarNode))

  // Parsing rules

  /*
   Assigns an expression.
   Example of literal assignment: X = 5
   Example of expression assignment: X = 3 + 6
   */
  private val varRule = P(eol ~ ident ~ AssignmentChar ~ expr ~ eol)

  /*
   Prints either a variable or an expression
   Example of variable printing: print X
   Example of expression printing: print 5 + 3
   */
  private val printRule = P(eol ~ Print ~ (ident | expr) ~ eol)

  // Statements
  private val assignmentStatement = varRule.map(v => AssignmentStatement(v._1, v._2))
  private val printStatement      = printRule.map(PrintStatement)
  private val parsedStatement     = P(assignmentStatement | printStatement)

  private def evalExpression(tree: (Int, Seq[(String, Int)])): Int = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) =>
        op match {
          case "+" => left + right
          case "-" => left - right
          case "*" => left * right
          case "/" => left / right
        }
    }
  }

  def parseLines(lines: Seq[String]): Program =
    lines
      .map((s: String) => parsedStatement.parse(s))
      .collect {
        case stmt: fastparse.core.Parsed.Success[Statement, _, _] => stmt.value
      }
      .foldLeft(Program(Vector.empty[Statement]))(
        (coll, statement) => Program(coll.statements :+ statement)
      )

}
