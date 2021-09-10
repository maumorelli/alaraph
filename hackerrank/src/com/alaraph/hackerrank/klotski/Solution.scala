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
 * This is my solution to problem: https://www.hackerrank.com/challenges/klotski/problem
 * mauro@alaraph.com
 */

object Solution {

  import scala.io.StdIn.readLine

  case class Cell(y: Int, x: Int) {
    def applyOffset(o: Offset) = Cell(y + o.dy, x + o.dx)
    override def toString = "(%d,%d)".format(y, x)
  }

  case class Offset(dy: Int, dx: Int) {
    def invert = Offset(-dy, -dx)
  }

  case class Block(symbol: String, cells: Set[Cell]) {
    private def minmaxXY = {
      val xx = cells.map { case Cell(_, x) => x }
      val yy = cells.map { case Cell(y, _) => y }
      (xx.min, xx.max, yy.min, yy.max)
    }

    val (minX, maxX, minY, maxY) = minmaxXY
    val uL = Cell(minY, minX)
    val bR = Cell(maxY, maxX)
    val width = maxX - minX + 1
    val height = maxY - minY + 1

    override def hashCode =
      (symbol + (scala.collection.immutable.SortedSet[(Int,Int)]() ++ cells.map (c => (c.y, c.x))).toString).hashCode
  }

  case class Move(block: String, from: Cell, to: Cell) {
    def offset = Offset(to.y - from.y, to.x - from.x)
  }

  case class Board(blocks: Map[String, Block], maxX: Int, maxY: Int) {
    val minX = 0
    val minY = 0
    val symbols = blocks.keys

    def perform(s: String, o: Offset) = {
      val b = blocks(s)
      val cells = b.cells.map(c => c.applyOffset(o))
      val outOfBound = cells.find { case Cell(y, x) => y < 0 || y > maxY || x < 0 || x > maxX }

      outOfBound match {
        case Some(_) => None
        case None => val otherBlocks = blocks - s
          val overlap = otherBlocks.values.find(_.cells.intersect(cells) != Set.empty)
          overlap match {
            case Some(_) => None
            case None => Some(Board(blocks + (s -> Block(s, cells)), maxX, maxY))
          }
      }
    }
    override def hashCode = {
      val ccat: (String, String) => String = (x, y) => x.concat(" ".concat(y))
      blocks.values.map (b => b.hashCode).toList.sorted.map(x => x.toString).foldLeft("")(ccat).hashCode
    }
    override def toString = {
      val block = "." * blocks.keys.head.size
      val board = Array.fill(maxY+1, maxX+1)(block)
      blocks.values.foreach(b => b.cells.foreach(c => board(c.y)(c.x)= b.symbol))
      val ccat: (String, String) => String = (x, y) => x.concat(" ".concat(y))
      " " + board.toList.map(x => x.toList).map(y => y.foldRight("\n")(ccat)).foldRight("")(ccat) + "\n"
    }
  }

  object Board {
    def build(b: Vector[Vector[String]]) = {
      val maxY = b.size - 1
      val maxX = b(0).size - 1
      val eb = "." * b(0)(0).length //empty block
      val flatb = for { i <- 0 to maxY
                        j <- 0 to maxX
                        if b(i)(j) != eb
      } yield (b(i)(j), Cell(i,j))

      Board(flatb.groupBy(_._1).map {
        case (y, zz) => (y, Block(y, zz.map(_._2).toSet))
      }, maxX, maxY)
    }
  }

  def isSolved(symbol:String, target:Cell, board:Board) =
    board.blocks(symbol).uL == target

  type Path = List[Move]
  val hist = scala.collection.mutable.SortedSet[Int]()

  def solve(brd:Board, block:String, target:Cell, maxmoves: Int = 250):Path = {
    val symbols = (brd.symbols.toSet - block).toList

    val offsets = List(Offset(-1, 0), Offset(1, 0), Offset(0, -1), Offset(0, 1))
    case class Trail(board: Board, path: Path)
    type Trails = List[Trail]
    type Offsets = List[Offset]

    def pursueTarget(trails: Trails, acc: List[Trail] = Nil): (Trails, Path) = trails match {
      case t :: tx => if (isSolved(block, target, t.board)) (trails, t.path)
      else {
        val _trails = moveBlockAroundOnce(t, block, offsets)
        pursueTarget(tx ++ _trails, acc ++ _trails)
      }
      case Nil => (acc, Nil)
    }

    def moveBlockAroundOnce(trail: Trail, s: String, offsets: Offsets, acc: Trails = Nil): Trails = offsets match {
      case o :: ox => trail.board.perform(s, o) match {
        case Some(newBoard) => val hashcd = newBoard.hashCode
                               if (hist.contains(hashcd))
                                  moveBlockAroundOnce(trail, s, ox, acc)
                               else {
                                 hist.add(hashcd)
                                 moveBlockAroundOnce(trail, s, ox, Trail(newBoard,
                                 Move(s, trail.board.blocks(s).uL, newBoard.blocks(s).uL) :: trail.path)::acc)
                               }
        case None => moveBlockAroundOnce(trail, s, ox, acc)
      }
      case Nil => acc
    }

    def moveBlockAround(trails: Trails, s: String, acc: Trails = Nil): Trails = trails match {
      case t :: tx => val _trails = moveBlockAroundOnce(t, s, offsets)
        moveBlockAround(tx ++ _trails, s, acc ++ _trails)
      case Nil => acc
    }

    def checkOneTrail(trail: Trail, symbols: List[String], acc: Trails = Nil): Trails = symbols match {
            case s :: sx => val _trails = moveBlockAround(List(trail), s)
                            checkOneTrail(trail, sx, acc ++ _trails)
            case Nil => acc
      }

    def _solve(trails: Trails): Path = trails match {
        case t :: tx => //println("Path.size=" + prepare(t.path).size + ", Hist.size=" + hist.size + ", Trails.size=" + trails.size)
                        val (_trails, _path) = if (t.path == Nil || t.path.head.block != block) pursueTarget(List(t))
                                               else (Nil, Nil)
                        if (_path != Nil) _path
                        else {
                          val _symbols = if (t.path == Nil) symbols
                                         else (symbols.toSet - t.path.head.block).toList
                          val newTrails = checkOneTrail(t, _symbols)
                          _solve(tx ++ newTrails ++ _trails)
                        }
        case Nil => Nil
    }


    if (isSolved(block, target, brd)) Nil
    else {
      hist.add(brd.hashCode)
      val trails = List(Trail(brd, Nil))
      if (!symbols.isEmpty) _solve(trails) else pursueTarget(trails)._2
    }
  }

  def prepare(path:Path):Path = path match {
    case m1::m2::tail => if (m1.block == m2.block) prepare(Move(m1.block, m2.from, m1.to)::tail)
    else m1::prepare(m2::tail)
    case _ => path
  }

  def main(args: Array[String]): Unit = {
    val Array(m,n) = readLine.split(' ').map(_.toInt)
    val puzzle = (for (i <- 1 to m) yield readLine.split(' ').toVector).toVector
    val block = readLine
    val target = readLine.split(' ').map(_.toInt)
    val res = solve(Board.build(puzzle), block, Cell(target(0),target(1)))
    val path = prepare(res).reverse
    println(path.length)
    path.foreach(m => printf("%s %s %s\n", m.block, m.from, m.to))
  }
}
