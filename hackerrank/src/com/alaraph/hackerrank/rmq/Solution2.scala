package com.alaraph.hackerrank.rmq

/**
 * @author mauromorelli
 */
 
object Solution2 {
  

case class Range(l: Int, r: Int) {
  require (l<=r)
  def intersect(rg: Range):Option[Range] =
    if (this.r < rg.l || this.l > rg.r) None 
    else Some(Range(math.max(this.l, rg.l), math.min(this.r, rg.r)))

  final def &(rg:Range) = intersect(rg)
  
  def >=(rg: Range):Boolean =
    if (this.l <= rg.l && rg.r <= this.r) true
    else false 
    
  def >(rg: Range):Boolean =
    if (this != rg && (this >= rg)) true else false
      
  override def toString: String = "(%d, %d)".format(l, r)
}

abstract class SegmentTree[V](val rg: Range, val v: V) {
  def eval(r: Range): V
  final def eval(l:Int, r:Int):V = eval(Range(l, r))
}

class Leaf[V](i: Int, v: V) extends SegmentTree(Range(i, i), v) {
  override def toString: String = "(range=%s, value=%s)".format(rg, v)
  override def eval(rg:Range) = {
    require(rg == this.rg)                             
    v
  }
}

class Node[V](rg: Range, 
              v: V,
              val fun: (V,V) => V,
              val left: SegmentTree[V], 
              val right: SegmentTree[V]) extends SegmentTree(rg, v) {
 
  override def toString: String = 
    "(range=%s, value=%s, \nleft=%s, \nright=%s)".
      format(rg, v, left, right)

  override def eval(rg:Range) = {
    require(this.rg >= rg)
    if (rg == this.rg) v
    else ((left.rg intersect rg, right.rg intersect rg): @unchecked) match {
        case (None, Some(r)) => right eval r                             
        case (Some(l), None) => left eval l                            
        case (Some(l), Some(r)) => fun(left eval l, right eval r)                             
      }                           
   } 
}

  object SegmentTree {
    def build[V](data: Vector[V], fun:(V,V)=>V): SegmentTree[V] = {
      def buildAcc(l: Int, r: Int): SegmentTree[V] =
        if (l == r)
          new Leaf(l, data(l))
        else {
          val n = (l + r) / 2
          val left = buildAcc(l, n)
          val right = buildAcc(n + 1, r)
          new Node(Range(l, r), fun(left.v, right.v), fun, left, right)
        }
      buildAcc(0, data.length - 1)
    }
  }
 
  import scala.io.StdIn.readLine
  def readInts: Vector[Int] = readLine.split(' ').map(_.toInt).toVector

  def main(args: Array[String]): Unit = {
    val Vector(nelem, nqueries) = readInts
    val sTree = SegmentTree.build(readInts, math.min)
    val queries = for (i <- 1 to nqueries)
                  yield readInts
    queries.foreach(x => println(sTree.eval(x(0), x(1))))
  }
}
