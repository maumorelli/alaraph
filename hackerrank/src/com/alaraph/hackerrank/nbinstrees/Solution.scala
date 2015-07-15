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
 * This is my solution to problem: https://www.hackerrank.com/challenges/number-of-binary-search-tree
 * mauro@alaraph.com
 */

package com.alaraph.hackerrank.nbinstrees

import scala.io.StdIn.readLine

object Solution {

  
   def main(args: Array[String]) {
     
     val nLines = readLine.toInt
     require(1 <= nLines && nLines <= 1000)
     
     val input = (for (i <- 1 to nLines) yield readLine) map(_.toInt)
     
     input foreach {
       x => require (1 <= x && x <= 1000)
       println(g(x) % (BigInt(10).pow(8)+7))
     }
   }
  
   val cache = collection.mutable.Map[Int, BigInt]()
  
   def g(N: Int): BigInt = {
      
       def f(j: Int, acc: BigInt): BigInt =
            j match {
                case N => acc+g(N-1)
                case 1 => f(j+1, acc+g(N-1))
                case _ => f(j+1, acc+g(j-1)*g(N-j))
            }
      
       N match {
           case 1 => 1
           case _ => cache getOrElseUpdate(N, f(1, 0))
       }
   }
}
 