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
 * This is my solution to problem: https://www.hackerrank.com/challenges/stocks-prediction
 * mauro@alaraph.com
 */


package com.alaraph.hackerrank.stockpred5

/**
 * @author mauromorelli
 */

object Solution {

  case class Range(l: Int, r: Int) {
    require (l<=r)
    def intersect(rg: Range):Option[Range] =
      if (this.r < rg.l || this.l > rg.r) None
      else Some(Range(math.max(this.l, rg.l), math.min(this.r, rg.r)))

    final def &(rg:Range) = intersect(rg)

    def +(rg:Range) = if ((this & rg) != None ||
                          (this.r == rg.l - 1 || rg.r == this.l -1))
                        Some(Range(math.min(this.l, rg.l), math.max(this.r, rg.r)))
                      else None

    def >=(rg: Range):Boolean =
      if (this.l <= rg.l && rg.r <= this.r) true
      else false

    def >(rg: Range):Boolean =
      if (this != rg && (this >= rg)) true else false

    def len = r - l + 1

    override def toString: String = "(%d, %d)".format(l, r)
  }

  abstract class SegmentTree[V](val rg: Range, val v: V) {
    def eval(r: Range): V
    final def eval(l:Int, r:Int):V = eval(Range(l, r))
    def maxsub(ix: Int, p:(V)=>Boolean):Option[Range]

    protected[Solution] def maxsub(ix: Int,
               root: Node[V],
               parent: Node[V],
               p:(V)=>Boolean,
               lG:Int,
               rG:Int):Option[Range] = {
      val ixR = Range(ix, ix)
      require(rg >= ixR)

      if (p(v)) 
          if (rg == Range(lG,rG)) Some(rg)
          else {
            val rng = if (parent.left == this) {
                        val rL = if (rg.l > lG) 
                          root.maxsub(rg.l-1, root, root, p, lG, rg.l-1) 
                          else None
                        val rR = parent.right.maxsub(rg.r+1, parent, parent, 
                                                     p, parent.right.rg.l, 
                                                     parent.right.rg.r)
                        (rL, rR)
                      } else {
                        val rL = parent.left.maxsub(rg.l-1, parent, parent, 
                                                    p, parent.left.rg.l, 
                                                    parent.left.rg.r)
                        val rR = if (rg.r < rG) 
                          root.maxsub(rg.r+1, root, root, p, rg.r+1, rG) 
                          else None
                        (rL, rR)
                      }
            
            rng match {
              case (None, Some(y)) => rg + y
              case (Some(x), None) => rg + x
              case (Some(x), Some(y)) => (rg + x).flatMap(z => z + y)
              case (None, None) => Some(rg)
            }
          }
       else dig(ix,root,p,lG,rG)
     }
    
     protected[Solution] def dig(ix: Int, root: Node[V], p:(V)=>Boolean, 
                                 lG: Int, rG: Int): Option[Range]

  }

  class Leaf[V](i: Int, v: V) extends SegmentTree(Range(i, i), v) {
    override def toString: String = "(range=%s, value=%s)".format(rg, v)
    override def eval(rg:Range) = {
      require(rg == this.rg)
      v
    }

    override def maxsub(ix: Int, p:(V)=>Boolean):Option[Range] = {
      val ixR = Range(ix,ix)
      require(this.rg == ixR)
      if (p(v)) Some(ixR) else None
    }
    
    override protected[Solution] def dig(ix: Int, root: Node[V], 
                                         p:(V)=>Boolean, lG: Int, rG: Int) = None

  }

  class Node[V](rg: Range,
                v: V,
                val fun: (V,V) => V,
                val left: SegmentTree[V],
                val right: SegmentTree[V]) extends SegmentTree(rg, v) {
    override def toString: String = "(range=%s, value=%s)".format(rg, v)

    override def eval(rg:Range) = {
      require(this.rg >= rg)
      if (rg == this.rg) v
      else ((left.rg intersect rg, right.rg intersect rg): @unchecked) match {
          case (None, Some(r)) => right eval r
          case (Some(l), None) => left eval l
          case (Some(l), Some(r)) => fun(left eval l, right eval r)
      }
    }

    override protected[Solution] def dig(ix: Int, root: Node[V], 
                                         p:(V)=>Boolean, lG: Int, rG: Int) =
      (if (left.rg >= Range(ix,ix)) left else right).maxsub(ix, root, this, p, lG, rG)

    override def maxsub(ix: Int, p:(V)=>Boolean):Option[Range] = 
      maxsub(ix, this, this, p, rg.l, rg.r)
  }

  object SegmentTree {
    def build[X,V](data: Vector[X], fun:(V,V)=>V, xToV:(X)=>V): SegmentTree[V] = {
      def buildAcc(l: Int, r: Int): SegmentTree[V] =
        if (l == r)
          new Leaf(l, xToV(data(l)))
        else {
          val n = (l + r) / 2
          val left = buildAcc(l, n)
          val right = buildAcc(n + 1, r)
          new Node(Range(l, r), fun(left.v, right.v), fun, left, right)
        }
      buildAcc(0, data.length - 1)
    }
  }

  case class Value(min: Int, max: Int) {
    override def toString = "(min:%d, max:%d)".format(min, max)
  }

  def fun(v1: Value, v2: Value): Value = Value(math.min(v1.min, v2.min), 
                                               math.max(v1.max, v2.max))
  def intToValue(i: Int) = Value(i,i)

  import scala.io.StdIn.readLine
  import scala.io.StdIn.readInt
  def readInts: Vector[Int] = readLine.split(' ').map(_.toInt).toVector

  def g(a: Int, d: Int)(v: Value) = v.min >= a && v.max <= a+d

  def main(args: Array[String]): Unit = {
     val n = readInt
     val a = readInts
     require (a.length == n)
     val Q = readInt
     val queries = for {
       i <- 0 until Q
     } yield readInts
     require(queries.forall { _.length == 2 })
     val s = SegmentTree.build(a, fun, intToValue)
     val res = queries.map { case Vector(ix, m) => s.maxsub(ix, g(a(ix), m))}
     res foreach { x => x match {
                          case Some(r) => println(r.len)
                          case None => 
                        } 
                 }
  }
}
