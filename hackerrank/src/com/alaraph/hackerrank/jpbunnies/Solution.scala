package com.alaraph.hackerrank.jpbunnies

/**
 * @author mauromorelli
 */

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