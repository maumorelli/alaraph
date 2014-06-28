/**
 *   Copyright 2014 www.alaraph.com
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */
package com.alaraph.sudoku

object SudokuApp {

  def main(args: Array[String]): Unit = {
    val board = readBoard(args)
    if (board != Vector.empty)
      solve(board)
    else
      instructions
  }

  private def readBoard(args: Array[String]): Vector[Vector[Char]] = {
    args.headOption match {
      case Some(str) =>
        val opt = str.takeWhile(_ != '=')
        val arg = str.substring(str.indexOf('=')+1)
        val actions = Map("-s" -> fromString _,
          "-f" -> fromFile _,
          "-c" -> fromConsole _) withDefaultValue ((_: String) => (Vector.empty))
        actions(opt)(arg)

      case None =>
        Vector.empty
    }
  }

  private def fromString(s: String) = {
    lazy val board = (for {
      (size, alpha) <- Sudoku.alphabet
      ncells = size*size
      filteredInput = s.filter(c => (alpha + '-').contains(c))
      if filteredInput.size == ncells
    } yield (size, filteredInput)).toSeq

    board.headOption match {
      case Some((size, input)) =>
        input.grouped(size).map(x => x.toVector).toVector
      case None =>
        val msg = for ((s, a) <- Sudoku.alphabet)
          yield "(*) %d symbols taken from %s".format(s * s, a.toSeq.sorted.mkString("(", ",", ")"))
        println(msg.mkString("\n"))
        Vector.empty
    }
  }

  private def fromFile(filename: String) = {
    /* This statement is taken from:
     * http://alvinalexander.com/scala/scala-how-open-read-files-scala-examples
     */
    import scala.io.Source
    val fileContents = Source.fromFile(filename).getLines.mkString
    fromString(fileContents)
  }

  private def fromConsole(size: String) = {
    try {
      val s = size.toInt
      if (!Sudoku.alphabet.keySet.contains(s)) {
        Vector.empty
      } else {
        val board = for (n <- 1 to s)
          yield Console.readLine
        fromString(board.mkString)
      }
    } catch {
      case e1: java.lang.NumberFormatException =>
        println("%s is not a valid size. Size can be %s".
          format(size, Sudoku.alphabet.keys.mkString("(", ",", ")")))
        Vector.empty
    }
  }

  private def instructions = {
    println("*********************************************************");
    println("                       SudokuApp                         ");
    println("*********************************************************");
    println(" Usage:                                                  ");
    println(" 1) SudokuApp -s=<string with Sudoku to be solved>       ");
    println(" 2) SudokuApp -f=<file with Sudoku to be solved>         ");
    println(" 2) SudokuApp -c=<size of Sudoku> //typed in the console ");
    println(" Remarks:                                                ");
    println(" *) In both uses, empty cells must be set to '-'         ");
    println(" *) The size of the alphabet can be 9(default), 4 or 16  ");
    println(" Examples:                                               ");
    println(" *) SudokuApp -f=./games/mySudo9.txt                     ");
    println(" *) SudokuApp -c=9                                       ");
    println(" *) SudokuApp -s=-1-4#----#----#9---#                    ");
    println("*********************************************************");
  }

  private def solve(board: Vector[Vector[Char]]) = {
    val sudo = Sudoku.build(board, board.size)
    println("Loaded Sudoku:\n%s".format(sudo))
    SudokuSolver solve sudo match {
      case Some(s) => println(println("Solved Sudoku:\n%s".format(s)))
      case None => println("No solution could be found")
    }
  }
}