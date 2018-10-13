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

object Ast {

  sealed trait Node

  case class VarNode(value: String)  extends Node
  case class OperandNode(value: Any) extends Node

  sealed trait Expression                                            extends Node
  case class PlusExpression(left: Expression, right: Expression)     extends Expression
  case class MultiplyExpression(left: Expression, right: Expression) extends Expression
  case class MinusExpression(left: Expression, right: Expression)    extends Expression
  case class DivideExpression(left: Expression, right: Expression)   extends Expression
  case class BoolExpression(left: Expression, right: Expression)     extends Expression
  case class IntLiteral(value: Int)                                  extends Expression
  case class BoolLiteral(value: Boolean)                             extends Expression

  sealed trait Statement                                                   extends Node
  case class AssignmentStatement(varNode: VarNode, expression: Expression) extends Statement
  case class ArithmeticStatement(expression: Expression)                   extends Statement
  case class BoolStatement(expression: Expression)                         extends Statement
  case class PrintStatement(expression: Expression)                        extends Statement

  case class ErrorStatement(error: String) extends Statement
  case class Program(statements: Seq[Statement])
}
