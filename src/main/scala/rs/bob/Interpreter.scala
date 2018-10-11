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
import rs.bob.Ast._

object Interpreter {

  private var variables = Map.empty[String, Any]

  def evaluate(program: Program): Unit =
    program.statements.foreach {
      case AssignmentStatement(VarNode(ident), node) =>
        variables = variables + (ident -> node.value)
        println(ident + ": " + node.value)
      case PrintStatement(VarNode(ident)) =>
        println(variables.getOrElse(ident, s"Variable $ident not defined."))
      case PrintStatement(node)  => println(node.value)
      case ExprStatement(node)   => println(node.value)
      case ErrorStatement(error) => println(error)
      case _                     => throw new Exception("Unknown statement type")
    }
}
