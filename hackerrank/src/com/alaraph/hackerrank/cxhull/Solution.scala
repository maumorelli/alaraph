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
 * This is my solution to problem: https://www.hackerrank.com/challenges/convex-hull-fp
 * mauro@alaraph.com
 */
package com.alaraph.hackerrank.cxhull

object ConvexHull {


  def grahamScan(points: Vector[(Int, Int)]): Double = {

    /*
     * Algorithm theta has been taken from
     * Robert Sedgevik's "Algorithms in C", Graham Scan
    */

    def theta(p0: (Int, Int), p1: (Int, Int)): Double = {
      val dx = p1._1 - p0._1
      val dy = p1._2 - p0._2
      val ax = math.abs(dx)
      val ay = math.abs(dy)
      val t = if (ax + ay == 0) 0.0 else dy.toFloat / (ax + ay)
      90.0 * (if (dx < 0) 2.0 - t else if (dy < 0) 4.0 + t else t)
    }

    def distance(p1: (Int, Int), p2: (Int, Int)) =
      math.sqrt(math.pow(p2._1 - p1._1, 2) + math.pow(p2._2 - p1._2, 2))

    def perimeter(ps: List[(Int, Int)]) = {
      def perimeterAcc(ps: List[(Int, Int)], acc: Double): Double =
        ps match {
          case p1 :: (p2 :: pt) => perimeterAcc(p2 :: pt, acc + distance(p1, p2))
          case _ => acc
        }
      perimeterAcc(ps, 0.0)
    }

    def scan(points: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
      def ccw(p0: (Int, Int), p1: (Int, Int), p2: (Int, Int)): Integer =
        (p1._1 - p0._1) * (p2._2 - p0._2) - (p1._2 - p0._2) * (p2._1 - p0._1)
      (points, acc) match {
        case (Nil, _) => acc
        case (p :: ps, p2 :: p1 :: pps) => if (ccw(p1, p2, p) >= 0) scan(ps, p :: acc)
        else scan(points, p1 :: pps)
        case (p :: ps, xs) => scan(ps, p :: xs)
      }
    }

    val start = points.min(Ordering[(Int, Int)].on { x: (Int, Int) => (x._2, x._1) })
    val thetas = points.map(p => (p, theta(start, p), distance(p, start)))
    perimeter(start::scan(thetas.sortBy(t => (t._2, t._3)).map { case (p, t, d) => p }.toList, Nil))
  }

  def main(args: Array[String]): Unit = {
    val n = readLine.toInt
    val input = (for (i <- 1 to n)
      yield (readLine split ("""\s""")).map(_.toInt)).map { case Array(a, b) => (a, b) }.toVector
    println("%.1f".format(grahamScan(input)))
  }
}