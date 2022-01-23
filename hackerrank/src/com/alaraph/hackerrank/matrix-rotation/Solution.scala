/**
 *   Copyright 2022 www.alaraph.com
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

package com.alaraph.hackerrank.matrix_rotation
import scala.io.StdIn.readLine

object Solution {

  def m2ring(matrix: Vector[Vector[Int]])(offset:Int): Vector[(Int, Int)] = {
    val m = matrix.size
    val n = matrix(0).size
    val north = (0 + offset to n - 1 - offset).map(j => (offset, j)).toVector
    val east = (1 + offset to m - 2 - offset).map(j => (j, n - 1 - offset)).toVector
    val south = (n - 1 - offset to 0 + offset by -1).map(j => (m - 1 - offset, j)).toVector
    val west = (m - 2 - offset to 1 + offset by -1).map(j => (j, offset)).toVector
    north ++ east ++ south ++ west
  }
  def m2rings(matrix: Vector[Vector[Int]]) = {
    val m = matrix.size
    val n = matrix(0).size
    val g = m2ring(matrix)(_)
    val rings = (0 to Math.min(m,n)/2-1).map(i => g(i))
    val coords = (0 until rings.size).flatMap ( i => (0 until rings(i).size).map ( j => rings(i)(j) -> (i, j))).toMap
    (rings, coords)
  }

  def perform(matrix: Vector[Vector[Int]], times: Int) = {
    val (rings, coords) = m2rings(matrix)
    def rotateMatrix(lmat: List[Vector[Int]], acc: Vector[Vector[Int]], row: Int = 0): Vector[Vector[Int]] = lmat match {
      case Nil => acc
      case r::rr =>
        val rotatedRow = (0 until r.size ).map { case col =>
                      val (rix, vix) = coords.getOrElse((row, col), (0,0))
                      val (_r, _c) = rings(rix)((vix+times) % rings(rix).size)
                      matrix(_r)(_c)
                    }.toVector

        rotateMatrix(rr, acc :+ rotatedRow, row + 1)
    }
    rotateMatrix(matrix.toList, Vector.empty, 0)
  }

  def mToString(matrix: Vector[Vector[Int]]): String = {
    val (nRows, nCols) = (matrix.size, matrix(0).size)
    val str = for {
      r <- 0 until nRows
      c <- 0 until nCols
      sep = if ((c + 1) % nCols == 0) "\n" else " "
    } yield "%s%s".format(matrix(r)(c), sep)
    str.mkString("")
  }

  def main(args: Array[String]): Unit = {
    val Array(m, n, r) = readLine.split(' ').map(_.toInt)
    require(Math.min(n,m) % 2 == 0, "Error: min(m,n) must be odd.")
    val matrix =  (1 to m).map(_ => readLine.split(' ').map(_.toInt).toVector).toVector
    val res = perform(matrix, r)
    println(mToString(res))
  }
}
