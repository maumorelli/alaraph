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

/*
 * This is my solution to problem:
 * https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles
 * mauro@alaraph.com
 */

package com.alaraph.hackerrank.sierpinski

object Solution {

  def drawTriangles(N: Int) = {
    type Matrix = Vector[Vector[Char]]
    
    def sieve(row: Int, offset: Int, base: Int, c: Char, matrix: Matrix): Matrix =
      if (base > 0)
        sieve(row + 1, offset + 1, base - 2, c, matrix.updated(row, matrix(row).patch(offset, c.toString * base, base)))
      else matrix

    def init: Matrix =
      sieve(0, 0, 63, '1', (for (i <- 1 to 32) yield ("_" * 63).toVector).toVector).reverse

    def drawTrianglesAcc(n: Int, row: Int, offset: Int, base: Int, acc: Matrix): Matrix = {
      if (n < N)
        drawTrianglesAcc(n + 1,
          row + base / 4 + 1,
          offset + base - base / 4,
          base / 2,
          drawTrianglesAcc(n + 1,
            row + base / 4 + 1,
            offset - base / 4 - 1,
            base / 2,
            drawTrianglesAcc(n + 1,
              row - base / 4 - 1,
              offset + base / 4 + 1,
              base / 2,
              sieve(row, offset, base, '_', acc))))
      else acc
    }

    if ((0 to 5).toSet.contains(N))
      print(drawTrianglesAcc(0, 16, 16, 31, init).map(_.mkString("", "", "")).mkString("", "\n", ""))
    else
      println("Invalid parameter value N=%d".format(N))
  }

  def main(args: Array[String]) {
    drawTriangles(readInt())
  }
  
}