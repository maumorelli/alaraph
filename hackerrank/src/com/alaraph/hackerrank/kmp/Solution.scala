/**
 *   Copyright 2019 www.alaraph.com
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
 * https://www.hackerrank.com/challenges/kmp-fp/problem
 * mauro@alaraph.com
 */

object Solution {
  import scala.io.StdIn.readLine
  
  def kmp(p:String, t:String): String = {
    val text = t.toVector
    val path = p.toVector
    val pathlen = path.length
    val textlen = text.length
    
    def next:Vector[Int] = {
      def _next(i: Int, j:Int, acc: Vector[Int]):Vector[Int] = {
       def nextJ(j: Int):Int =
         if (j >= 0 && path(i) != path(j)) nextJ(acc(j))
         else j
        
  		   if (i < pathlen) _next(i+1, nextJ(j) + 1, acc :+ j)
  		   else acc
      }
      _next(0, -1, Vector.empty)
    }
    
    val nextpos = next
    
    def _kmp(i:Int, j:Int): String = {
       def nextJ(j: Int):Int =
         if (j >= 0 && text(i) != path(j)) nextJ(nextpos(j))
         else j
      
       if (j == pathlen) "YES"
       else if (i < textlen) _kmp(i+1, nextJ(j) + 1)
       else "NO"
    }   
    _kmp(0,0)   
  }

  def main(args: Array[String]): Unit = {
    val n = readLine.toInt
    
    for (i <- 0 until n) {
      val txt = readLine()
      val pat = readLine()
      println(kmp(pat, txt))
    }
  }
}
