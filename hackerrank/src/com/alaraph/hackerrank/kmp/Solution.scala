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
  
  def next(j:Int, c:Char, pat:Vector[Char], offset:Vector[Int]):Int = 
    if (j >= 0 && c != pat(j)) next(offset(j), c, pat, offset) else j
    
  def offsets(pat:Vector[Char]):Vector[Int] = {
      def _offsets(i: Int, j:Int, acc: Vector[Int]):Vector[Int] =    
              if (i < pat.length) _offsets(i + 1, next(j, pat(i), pat, acc) + 1, acc :+ j)
              else acc
      
      _offsets(0, -1, Vector.empty)
  }
  
  def kmp(path:String, text:String): String = {
    val t = text.toVector
    val p = path.toVector
    val offs = offsets(p)
    
    def _kmp(i:Int, j:Int): String = 
       if (j == p.length) "YES"
       else if (i < t.length) _kmp(i+1, next(j, t(i), p, offs) + 1)
       else "NO"
      
    _kmp(0,0)   
  }

  def main(args: Array[String]): Unit = 
    for (i <- 0 until readLine.toInt) {
      val txt = readLine()
      val pat = readLine()
      println(kmp(pat, txt))
    }
}
