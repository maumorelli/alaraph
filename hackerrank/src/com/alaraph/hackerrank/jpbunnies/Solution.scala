/**
 *   Copyright 2015 www.alaraph.com
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
 * This is my solution to problem: https://www.hackerrank.com/challenges/jumping-bunnies
 * mauro@alaraph.com
 */

package com.alaraph.hackerrank.jpbunnies

object Solution {
   def main(args: Array[String]) {
      val n = scala.io.StdIn.readInt
      val jumps = scala.io.StdIn.readLine.split(" ").map(_.toInt).sortWith((X, Y) => X>Y)
      require(jumps.size == n)
      println(mcm(jumps.toList))
   }
  def p(n: Int, i: Int): (Int, Int) = {
    require(n >= i & i > 1)
    def pA(m: Int, acc: Int = 0): (Int, Int) = m % i match {
      case 0 => pA(m / i, acc + 1)
      case _ => (acc, m)
    }
    pA(n, 0)
  }  
  def dec(n: Int): List[(Int,Int)] = {
    def decA(m: Int, i: Int, acc: List[(Int,Int)]): List[(Int,Int)] =
      if (i > m) acc else p(m, i) match {
          case (0, _) => decA(m, i+1, acc)
          case (e, r) => decA(r, i+1, (i,e)::acc)
      }
    decA(n, 2, Nil)
  }
  def mcm(nn: List[Int]): BigInt = {
    val dd = nn.flatMap((N=>dec(N)))
    dd.map(X =>X._1).distinct.map(A => dd.filter(X => X._1 == A).max).map(Y => BigInt(Y._1).pow(Y._2)).reduce((A,B) => A*B)
  }
}