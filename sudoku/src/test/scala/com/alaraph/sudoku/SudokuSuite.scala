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
package com.alaraph.sudoku

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class SudokuSuite extends FunSuite {
    val board1 = Vector(
                  Vector('-','-','5','-','6','-','1','-','-'),
                  Vector('-','3','-','7','-','2','-','9','-'),
                  Vector('6','-','-','-','-','-','7','-','-'),
                  Vector('-','9','-','-','-','7','-','-','-'),
                  Vector('-','-','4','-','-','-','6','-','-'),
                  Vector('-','-','-','3','-','-','-','8','-'),
                  Vector('-','-','9','-','-','-','-','-','3'),
                  Vector('-','6','-','2','-','9','-','7','-'),
                  Vector('-','-','7','-','1','-','5','-','-')
                )  
     val result1 = Vector(
                  Vector('9','7','5','8','6','4','1','3','2'), 
                  Vector('1','3','8','7','5','2','4','9','6'), 
                  Vector('6','4','2','9','3','1','7','5','8'), 
                  Vector('2','9','6','4','8','7','3','1','5'), 
                  Vector('3','8','4','1','9','5','6','2','7'), 
                  Vector('7','5','1','3','2','6','9','8','4'), 
                  Vector('4','1','9','5','7','8','2','6','3'), 
                  Vector('5','6','3','2','4','9','8','7','1'), 
                  Vector('8','2','7','6','1','3','5','4','9'))
  
     val board2 = Vector(
                  Vector('-','-','5','-','6','-','1','-','-'),
                  Vector('-','3','-','7','-','2','-','9','-'),
                  Vector('6','-','-','-','-','-','7','-','-'),
                  Vector('-','9','-','-','-','7','-','-','-'),
                  Vector('-','-','4','-','-','-','6','-','-'),
                  Vector('-','-','-','3','-','-','-','8','-'),
                  Vector('-','-','9','-','-','-','-','-','3'),
                  Vector('-','6','-','2','-','9','-','7','-'),
                  Vector('-','-','7','-','1','-','5','-','-')
                )  

     val board3 = "--5-6-1--"+
                  "-3-7-2-9-"+
                  "6-----7--"+
                  "-9---7---"+
                  "--4---6--"+
                  "---3---8-"+
                  "--9-----3"+
                  "-6-2-9-7-"+
                  "--7-1-5--"

                
                
  test("solve Sudoku 1") {
    val sudo = Sudoku.build(board1)
    assert(SudokuSolver.solve(sudo) === Some(Sudoku.build(result1)))
  }
    
  test("solve Sudoku 1 from string board") {
    val sudo = Sudoku.build(board3)
    assert(SudokuSolver.solve(sudo) === Some(Sudoku.build(result1)))
  }    
  
  test("sudo1(board1) equals sudo2(board1)") {
    val sudo1 = Sudoku.build(board1)
    val sudo2 = Sudoku.build(board1)
    assert(sudo1 === sudo2)
  }
  
  test("sudo1 equals sudo1") {
    val sudo1 = Sudoku.build(board1)
    assert(sudo1 === sudo1)
  }
  
  test("sudo1_b1.hashCode == sudo2_b1.hashCode") {
    val sudo1 = Sudoku.build(board1)
    val sudo2 = Sudoku.build(board1)
    assert(sudo1.hashCode === sudo2.hashCode)    
  }
  
  test("sudo1_b1.hashCode == sudo2_b2==b1.hashCode") {
    val sudo1 = Sudoku.build(board1)
    val board2 = board1
    val sudo2 = Sudoku.build(board2)
    assert(sudo1.hashCode === sudo2.hashCode)    
  }  
  
  test("sudo1_board1.hashCode != sudo2_result1.hashCode") {
    val sudo1 = Sudoku.build(board1)
    val sudo2 = Sudoku.build(result1)
    assert(sudo1.hashCode != sudo2.hashCode)    
  }  
  
  test("sudo1_b1.hashCode == sudo1_b1.hashCode") {
    val sudo1 = Sudoku.build(board1)
    assert(sudo1.hashCode === sudo1.hashCode)    
  }
  
  test("sudo1_board1.hashCode == sudo2_board2==board1.hashCode") {
    val sudo1 = Sudoku.build(board1)
    val sudo2 = Sudoku.build(board2)
    assert(sudo1.hashCode === sudo2.hashCode)    
  }  
  
}
