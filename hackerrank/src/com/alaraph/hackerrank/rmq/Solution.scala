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
 * This is my solution to problem: https://www.hackerrank.com/challenges/range-minimum-query
 * mauro@alaraph.com
 */

package com.alaraph.hackerrank.rmq

object Solution {
 
  abstract class SegmentTree(val range: (Int, Int), val min: Int) {
    final def query(l: Int, r: Int): Option[Int] = {
      require(l <= r)
      min(l, r)
    }
    protected[Solution] def min(l: Int, r: Int): Option[Int]
    protected[Solution] final def min(range: (Int, Int)): 
      Option[Int] = min(range._1, range._2)
  }

  class Leaf(ix: Int, min: Int) extends SegmentTree((ix, ix), min) {
    override def toString: String = "n=%d, min=%d".format(ix, min)
    override def min(l: Int, r: Int): Option[Int] =
      if (l == ix && r == ix) Some(min)
      else None
  }
 
  class Node(range: (Int, Int), 
             min: Int, 
             val left: SegmentTree, 
             val right: SegmentTree) extends SegmentTree(range, min) {
 
    override def toString: String = 
      "(range=(%d, %d), min=%d, \nleft=%s, \nright=%s)".
      format(range._1, range._2, min, left, right)

    override def min(l: Int, r: Int): Option[Int] = {
      def intersect(r1: (Int, Int), r2: (Int, Int)): (Int, Int) = {
        val ((l1, h1), (l2, h2)) = (r1, r2)
        require(l1 <= h1 && l2 <= h2)
        if (h1 < l2 || h2 < l1) (-1, -1)
        else (math.max(l1, l2), math.min(h1, h2))
      }

      val inter = intersect((l, r), range)
      if (inter == range) Some(min)
      else if (inter == (-1, -1)) None
      else {
        val lMin = left.min(intersect((l, r), left.range))
        val rMin = right.min(intersect((l, r), right.range))
        (lMin, rMin) match {
          case (None, rM) => rM
          case (lM, None) => lM
          case (Some(l), Some(r)) => Some(math.min(l, r))
        }
      }
    }
  }

  object SegmentTree {
    def build(data: Vector[Int]): SegmentTree = {
      def buildAcc(l: Int, r: Int, data: Vector[Int]): SegmentTree =
        if (l == r)
          new Leaf(l, data(l))
        else {
          val n = (l + r) / 2
          val left = buildAcc(l, n, data)
          val right = buildAcc(n + 1, r, data)
          new Node((l, r), math.min(left.min, right.min), left, right)
        }
      buildAcc(0, data.length - 1, data)
    }
  }
 
  def readInts: Vector[Int] = readLine.split(' ').map(_.toInt).toVector

  def main(args: Array[String]): Unit = {
    val Vector(nelem, nqueries) = readInts
    val sTree = SegmentTree.build(readInts)
    val queries = for (i <- 1 to nqueries)
                  yield readInts
    queries.foreach(x => sTree.query(x(0), x(1)).foreach(println(_)))
  }
}
