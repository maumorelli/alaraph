/**
 *   Copyright 2014 www.alaraph.com
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
 * This is my solution to problem: https://www.hackerrank.com/challenges/brainf_k-interpreter-fp
 * mauro@alaraph.com
 */

package com.alaraph.hackerrank.brainfuck

object Solution {
  // using Char because Byte is signed
  val mem = Array.fill[Char](300000)(0)
  var memPointer = 0
  var input = Vector[Char]()
  var inputPointer = -1
  var nExec = 0 // number of executed commands

  def incCounter: Unit =
    if (nExec < 100000)
      nExec = nExec + 1
    else {
      println("\nPROCESS TIME OUT. KILLED!!!")
      System.exit(0)
    }

  def left = memPointer = math.max(memPointer - 1, 0)
  def right = memPointer = memPointer + 1
  def dec = mem(memPointer) = if (mem(memPointer) > 0) (mem(memPointer) - 1).toChar  else 255
  def inc = mem(memPointer) = if (mem(memPointer) < 255) (mem(memPointer) + 1).toChar else 0
  def display = print("%c".format(mem(memPointer)))
  def load = {
    if (inputPointer != -1) {
      mem(memPointer) = input(inputPointer)
      inputPointer = if (inputPointer < input.length - 1)
        inputPointer + 1
      else -1
    }
  }

  def noop = {}

  def readInts: Vector[Int] = readLine.split(' ').map(_.toInt).toVector

  def readInput(len: Int): Unit = {
    val rawInput = readLine
    val input = rawInput.takeWhile(_ != '$')
    require(input.length == len)
    require(input.length + 1 == rawInput.length)
    if (input.length > 0) {
      Solution.input = input.toVector
      Solution.inputPointer = 0
    }
  }

  def readProg(nLines: Int): String = {
    val lines = for (i <- 1 to nLines) yield readLine
    lines.mkString("", "", "")
  }

  val commands = Map[Char, () => Unit](
    ('<', left _),
    ('>', right _),
    ('+', inc _),
    ('-', dec _),
    (',', load _),
    ('.', display _),
    ('[', noop _),
    (']', noop _)) withDefaultValue (noop _)

  def removeRemarks(in: String): String = in.filter(c => commands.keySet.contains(c))

  def run(prog: String): Unit = {
    def matchBracket(prg: Vector[Char], pinstr: Int): Int =
      if (Set('[', ']').contains(prg(pinstr)) == false) -1
      else {
        val bracket = prg(pinstr)
        val antibracket = if (bracket == '[') ']' else '['
        val (cond, step) = bracket match {
          case ']' => ((p: Int) => p > 0, (p: Int) => p - 1)
          case '[' => ((p: Int) => p < prg.length - 1, (p: Int) => p + 1)
        }
        var brkts = 1
        var p = pinstr
        while (cond(p) && brkts != 0) {
          p = step(p)
          if (prg(p) == bracket) brkts = brkts + 1
          else if (prg(p) == antibracket) brkts = brkts - 1
        }
        if (prg(p) == antibracket) p else -1
      } 
   
    val prg = removeRemarks(prog).toVector
   
    if (prg.length > 0) {
      var pinstr = 0
      while (pinstr < prg.length) {
        incCounter
        prg(pinstr) match {
          case '[' => if (mem(memPointer) > 0)
                        pinstr = pinstr + 1
                      else
                        pinstr = matchBracket(prg, pinstr)
      
          case ']' => if (mem(memPointer) > 0)
                        pinstr = matchBracket(prg, pinstr)
                      else
                        pinstr = pinstr + 1
      
          case c   => commands(prg(pinstr))()
                      pinstr = pinstr + 1                               
     
        }
      }  
    } 
  }

  def main(args: Array[String]): Unit = {
    val Vector(inputLen, nLines) = readInts
    require(inputLen >= 0 && inputLen <= 150)
    require(nLines >= 1 && nLines <= 150)
    readInput(inputLen)
    run(readProg(nLines))
  }
}
