
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
 * https://www.hackerrank.com/challenges/john-and-fences/problem
 * mauro@alaraph.com
 */


object Solution {
 
  import scala.io.StdIn.readLine

  def g(l:List[Int], h:Int, len: Int):Int = l match {
    case x::xx => if (x >= h) g(xx,h,len+1) else h*len
    case Nil => h*len
  }
    
  def f(ll:List[Int], rl:List[Int], acc: Int):Int = rl match {
    case x::xx => f(x::ll, xx, math.max(acc, g(xx,x,1)+g(ll,x,0)))
    case Nil => acc 
  }  
   
  def main(args: Array[String]): Unit = {
    val n = readLine.toInt
    val l = readLine.split(' ').map(_.toInt).toList
    val res = f(Nil, l, 0)
    println(res)
  }
}
