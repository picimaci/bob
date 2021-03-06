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

  case class VariableNode(varName: String) extends Node

  sealed trait Expression extends Node

  case class VariableLiteral(varName: String) extends Expression

  case class PlusExpression(left: Expression, right: Expression)     extends Expression
  case class MultiplyExpression(left: Expression, right: Expression) extends Expression
  case class MinusExpression(left: Expression, right: Expression)    extends Expression
  case class DivideExpression(left: Expression, right: Expression)   extends Expression

  case class OrExpression(left: Expression, right: Expression)  extends Expression
  case class AndExpression(left: Expression, right: Expression) extends Expression
  case class NotExpression(expression: Expression)              extends Expression

  case class GreaterThanExpression(left: Expression, right: Expression)       extends Expression
  case class LessThanExpression(left: Expression, right: Expression)          extends Expression
  case class GreaterThanEqualsExpression(left: Expression, right: Expression) extends Expression
  case class LessThanEqualsExpression(left: Expression, right: Expression)    extends Expression
  case class EqualsExpression(left: Expression, right: Expression)            extends Expression
  case class NotEqualsExpression(left: Expression, right: Expression)         extends Expression

  case class IntLiteral(value: Int)      extends Expression
  case class BoolLiteral(value: Boolean) extends Expression

  sealed trait Statement                                                        extends Node
  case object EmptyStatement extends Statement
  case class AssignmentStatement(varNode: VariableNode, expression: Expression) extends Statement
  case class SimpleStatement(expression: Expression)                            extends Statement
  case class PrintStatement(expression: Expression)                             extends Statement

  case class ErrorStatement(error: String) extends Statement
  case class Program(statements: Seq[Statement])
}
