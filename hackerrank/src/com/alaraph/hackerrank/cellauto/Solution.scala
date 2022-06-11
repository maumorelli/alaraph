package hackerrank.src.com.alaraph.hackerrank.cellauto

import hackerrank.src.com.alaraph.hackerrank.cellauto.Solution.Rule.{bits, ruleIndex}

import scala.annotation.tailrec
import scala.io.StdIn.{readInt, readLine}
import scala.math.pow


/**
 *   Copyright 2021 www.alaraph.com
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
 * This is my solution to problem: https://www.hackerrank.com/challenges/the-tree-of-life
 * mauro@alaraph.com
 */

object Solution {
    case class Cell(parent: Node, self: Node, left: Node, right: Node) {
      override def toString = parent.toString + left.toString + self.toString + right.toString
    }
    case class Rule(r: Char) {
      def applyTo(c: Cell): Node = if
      ((bits(ruleIndex.getOrElse(c, -1)) & r) > 0) Node.on else Node.off
    }
    object Rule {
      val nodeTypes = Vector(Node.off, Node.on)
      val sortedCells = nodeTypes.flatMap(n0 => nodeTypes.flatMap(n1 => nodeTypes.flatMap(n2 => nodeTypes.map(n3 => Cell(n0, n2, n1, n3)))))
      val ruleIndex: Map[Cell, Int] = sortedCells.indices.map(i => (sortedCells(i) -> i)).toMap
      val bits: Vector[Char] = (1 to 16).map(n => pow(2, n-1).toChar).toVector
      //val allOnes = pow(2, 17).toInt - 1
    }

    sealed trait Status
    object On extends Status
    object Off extends Status
    case class Node(s: Status) {
      override def toString = s match {
        case On => "X"
        case Off => "."
      }
    }
    object Node {
      val on = Node(On)
      val off = Node(Off)
    }

   case class BTree(n: Node, left:Option[BTree], right: Option[BTree]) {

     def applyRule(r: Rule, parent: Node = Node.off): BTree = {
       def nextCell(o: Option[BTree]) = o match {
         case Some(t) => t.n
         case None => Node.off
       }
       def subTree(o: Option[BTree]) = o match {
         case Some(t) => Some(t.applyRule(r, n))
         case None => None
       }
       BTree(r.applyTo(Cell(parent, n, nextCell(left), nextCell(right))), subTree(left), subTree(right))
     }
     @tailrec
     final def readNode(path: List[Char]): Node = path match {
       case Nil => n
       case h :: pp => val next = h match {
           case '<' => left
           case '>' => right
           case _ => None
         }
         next match {
           case Some(t) => t.readNode(pp)
           case None => n
         }
     }
   }
  object BTree {
    def apply(s:String): Option[BTree] =
      if (s.length < 7) parseNode(s) match {
          case (Some(n), _) => Some(BTree(n, None, None))
          case _ => None
      } else (s(0), s(s.length - 1)) match {
        case ('(', ')') =>
          val (left, lRem) = parseTree(s.substring(1, s.length - 1))
          val (node, nRem) = parseNode(lRem)
          val (right, rRem) = parseTree(nRem)
          (node, left, right) match {
            case (Some(n), Some(l), Some(r)) => Some(BTree(n, Some(l), Some(r)))
            case _ => None
          }
        case _ => None
      }
  }

  @tailrec
  def parseNode(s: String): (Option[Node], String) =
    if (s.isEmpty) (None, s) else s(0) match {
      case 'X' => (Some(Node.on), s.substring(1))
      case '.' => (Some(Node.off), s.substring(1))
      case ' ' => parseNode(s.substring(1))
      case _ => (None, s)
    }

  def parseTree(s: String): (Option[BTree], String) =
    if (s.isEmpty) (None, s) else parseNode(s) match {
      case (Some(n), rem) => (Some(BTree(n, None, None)), rem)
      case (None, _) => getTreeExpr(s) match {
        case (None, rem) => (None, rem)
        case (Some(exp), rem) => (BTree(exp), rem)
      }
    }

  @tailrec
  def getTreeExpr(s: String, expr: String = "", depth: Int=0): (Option[String], String) = {
    if (s.isEmpty) (None, expr + s) else s(0) match {
      case '(' => getTreeExpr(s.substring(1), expr + s(0), depth + 1)
      case ')' => if (depth == 1) (Some((expr + s(0)).trim), s.substring(1))
                     else getTreeExpr(s.substring(1), expr + s(0), depth - 1)
      case c => if (Set(' ', 'X', '.').contains(c))
                  getTreeExpr(s.substring(1), expr + s(0), depth)
                else (None, expr + s)
    }
  }

  def parseQuery(s: String): (Int, String) = {
    import scala.util.{Failure, Success, Try}
    val params = s.split(" ")
    if (params.length != 2) (0, "")
    else {
      val n = Try(params(0).toInt) match {
        case Success(n) => n
        case Failure(_)  => 0
      }
      val regex = "^\\[[<>]*\\]$".r
      val path = if (regex.matches(params(1))) params(1).substring(1, params(1).length-1)
                 else ""
      (n, path)
    }
  }
  case class Hist(ix: Int, hist: Vector[BTree])
  object Hist {
    def apply(t: BTree): Hist = Hist(0, Vector(t))
  }

  def perform(offset: Int, rule: Rule, h: Hist): Hist = {
    assert(h.hist.nonEmpty)
    @tailrec
    def run(times: Int, acc: Vector[BTree] = Vector.empty[BTree]): Vector[BTree] =
      if (times < 1) acc
      else run(times - 1, acc :+ acc.last.applyRule(rule))

    val newIx = h.ix + offset

    if (newIx < 0) h
    else if (newIx < h.hist.length) Hist(newIx, h.hist)
    else Hist(newIx, run(offset - (h.hist.length - h.ix - 1), h.hist))
  }

  @tailrec
  def processInput(nQueries: Int, rule: Rule, acc: Hist): Unit = if (nQueries > 0) {
      val (offset, path) = parseQuery(readLine())
      val h = perform(offset, rule, acc)
      println(h.hist(h.ix).readNode(path.toList).toString())
      processInput(nQueries - 1, rule, h)
  }


  def main(args: Array[String]): Unit = {
    val rule = Rule(readInt.toChar)
    BTree(readLine()) match {
      case Some(t) =>
        val nrQueries = readInt()
        assert(nrQueries > 0)
        processInput(nrQueries, rule, Hist(t))
      case None => System.exit(-1)
    }
  }
}
