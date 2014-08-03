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
 * https://www.hackerrank.com/challenges/fractal-trees
 * mauro@alaraph.com
 */
package com.alaraph.hackerrank.rectrees

object Solution {

  def drawTrees(N: Int) = {
    type Matrix = Vector[Vector[Char]]

    def drawY(row: Int, offset: Int, size: Int, matrix: Matrix): Matrix = {
      def drawYAcc(row: Int, n: Int, acc: Matrix): Matrix = 
        if (n < size / 2)
          drawYAcc(row - 1,
            n + 1,
            acc.updated(row,
              acc(row).updated(offset, '1')))
        else if (n < size)
          drawYAcc(row - 1,
            n + 1,
            acc.updated(row,
              acc(row).updated(offset - n + size / 2 - 1, '1').
                updated(offset + n - size / 2 + 1, '1')))
        else acc

      drawYAcc(row, 0, matrix)
    }

    def init: Matrix = (for (i <- 1 to 63) yield ("_" * 100).toVector).toVector

    def drawTreesAcc(n: Int, row: Int, offset: Int, size: Int, acc: Matrix): Matrix = 
      if (n <= N)
          drawTreesAcc(n + 1,
            row - size,
            offset + size / 2,
            size / 2,
            drawTreesAcc(n + 1,
              row - size,
              offset - size / 2,
              size / 2,
              drawY(row, offset, size, acc)))
      else acc
 

    if ((0 to 5).toSet.contains(N))
      print(drawTreesAcc(1, 62, 100 / 2 - 1, 16 * 2, init).map(_.mkString("", "", "")).mkString("", "\n", ""))
    else
      println("Invalid parameter value N=%d".format(N))
  }

  def main(args: Array[String]) {
    drawTrees(readInt())
  }

}