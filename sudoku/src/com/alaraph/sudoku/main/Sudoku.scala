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
package com.alaraph.sudoku.main

abstract class Sudoku(val board: Vector[Vector[Char]],
  val square: Int,
  val alphabet: Set[Char]) {
  val size = square * square
  val ncells = size * size

  require(board.size == size &&
    board.forall(row => row.size == size),
    "The board must have the same number of rows and columns: " + size)

  require(board.flatMap(row => row.toSet).toSet.subsetOf((alphabet + '-')),
    "The symbols in the board must be included in this set: %s".format(alphabet + '-'))

  override def toString: String = {
    val str = for {
      r <- 0 until size
      c <- 0 until size
      sep = if ((c + 1) % size == 0)
        "\n" + (if ((r + 1) % square == 0) "\n" else "")
      else if ((c + 1) % square == 0) " "
      else ""
    } yield "%c%s".format(board(r)(c), sep)

    str.mkString("")
  }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Sudoku]

  override def equals(other: Any): Boolean =
    other match {
      case that: Sudoku => (that canEqual this) &&
        (0 until size).forall(x => this.board(x) == that.board(x))
      case _ => false
    }

  override def hashCode: Int = board.hashCode
}

private class Sudoku9(board: Vector[Vector[Char]])
  extends Sudoku(board, 3, Sudoku.alphabet(9))
private class Sudoku4(board: Vector[Vector[Char]])
  extends Sudoku(board, 2, Sudoku.alphabet(4))
private class Sudoku16(board: Vector[Vector[Char]])
  extends Sudoku(board, 4, Sudoku.alphabet(16))

object Sudoku {

  def build(board: Vector[Vector[Char]], size: Int = 9): Sudoku = size match {
    case 9 => new Sudoku9(board)
    case 4 => new Sudoku4(board)
    case 16 => new Sudoku16(board)
    case _ => throw new IllegalArgumentException(sizeErrMsg)
  }

  def build(board: String): Sudoku = {
    val ncells = board.size
    val sizes = alphabet.keySet.map(x => (x * x, x)).toMap
    val size = sizes.get(ncells) 
    
    size match {
      case Some(s) => val vecBoard = (for (row <- board.sliding(s, s))
        yield row.toVector).toVector
        build(vecBoard, s)
      case None => throw new IllegalArgumentException(sizeErrMsg)
    }
}

  val alphabet = Map(
    (9 -> ('1' to '9').toSet),
    (4 -> ('1' to '4').toSet),
    (16 -> (('1' to '9') ++ ('A' to 'G')).toSet)).withDefaultValue(Set.empty)

  private val sizeErrMsg = "Sudoku can only have size %s".format(alphabet.keys.mkString(","))
}


