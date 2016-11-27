/**
 *   Copyright 2016 www.alaraph.com
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
 * This is an example showing how to use Type-Parametric Segment Trees
 * to compute the avg() function
 * mauro@alaraph.com
 */


package com.alaraph.util

/**
 * @author mauromorelli
 */
object STAvg {
  case class Value(avg: Float, len:Integer) {
    override def toString = "(avg:%1$.3f, len=%d)".format(avg, len)
  }
  
  def intToValue(i: Int) = Value(i, 1)
  def fun(v1: Value, v2: Value): Value = {
    val len = (v1.len + v2.len).toFloat
    Value((v1.len/len)*v1.avg+(v2.len/len)*v2.avg, v1.len + v2.len) 
  }
 
      
  import scala.io.StdIn.readLine
  def readInts: Vector[Int] = readLine.split(' ').map(_.toInt).toVector

  def main(args: Array[String]): Unit = {
    val Vector(nelem, nqueries) = readInts
    val sTree = SegmentTree.build(readInts, fun, intToValue)
    val queries = for (i <- 1 to nqueries)
                  yield readInts
    queries.foreach(x => println("%1.2f".format(sTree.eval(x(0), x(1)).avg)))
  }   
}



