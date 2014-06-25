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

object SudokuSolver {
  type Board = Vector[Vector[Char]]

  def solve(su: Sudoku): Option[Sudoku] = {
    def getOpts(r: Int, c: Int, b: Board): Set[Char] = {
      def getOpsAcc(i: Int, acc: Set[Char]): Set[Char] = {
        val z = su.size
        if (i < z) {
          val s = su.square
          val c1 = b((r + i) % z)(c)
          val c2 = b(r)((c + i) % z)
          val c3 = b((r / s * s) + (r + (i / s)) % s)((c / s * s) + (c + (i % s)) % s)
          getOpsAcc(i + 1, acc.diff(Set(c1, c2, c3)))
        } else acc
      }
      getOpsAcc(1, su.alphabet)
    }

    def next(k: Int, b: Board): Int = {
      lazy val cells = for {
        j <- (k until su.ncells)
        if b(j / su.size)(j % su.size) == '-'
      } yield j

      if (!cells.isEmpty) cells(0)
      else su.ncells
    }

    def solveAcc(k: Int, b: Board, opts: List[Char]): Option[Board] = {
      val j = next(k, b)
      if (j < su.ncells) {
        val (r, c) = (j / su.size, j % su.size)
        val options = if (opts == Nil) getOpts(r, c, b).toList
        else opts
        options match {
          case Nil => None
          case o :: os =>
            val sol = solveAcc(j + 1, b.updated(r, b(r).updated(c, o)), Nil)
            if (sol == None && !os.isEmpty) solveAcc(j, b, os)
            else sol
        }
      } else Some(b)
    }

    def boardIsValid: Boolean = {
      val b = su.board
      lazy val chars = for {
        r <- 0 until b.size
        c <- 0 until b.size
        char = b(r)(c)
        if char != '-'
      } yield (char, getOpts(r, c, b))
      chars.forall { case (c, cs) => cs.contains(c) }
    }

    require(boardIsValid, "The Sudoku board is not valid")
    solveAcc(0, su.board, Nil) match {
      case None => None
      case Some(b) => Some(Sudoku.build(b, su.size))
    }
  }

}