package com.alaraph.sudoku.main

object ws_01 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val msg = for ((s, a) <- Sudoku.alphabet)
    yield "(*) %d symbols taken from %s".format(s * s, a.toSeq.sorted.mkString("(", ",", ")"))
                                                  //> msg  : scala.collection.immutable.Iterable[String] = List((*) 81 symbols tak
                                                  //| en from (1,2,3,4,5,6,7,8,9), (*) 16 symbols taken from (1,2,3,4), (*) 256 sy
                                                  //| mbols taken from (1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,G))
  msg.mkString("\n")                              //> res0: String = (*) 81 symbols taken from (1,2,3,4,5,6,7,8,9)
                                                  //| (*) 16 symbols taken from (1,2,3,4)
                                                  //| (*) 256 symbols taken from (1,2,3,4,5,6,7,8,9,A,B,C,D,E,F,G)
  
  val board1 = "---------"*9                      //> board1  : String = ---------------------------------------------------------
                                                  //| ------------------------
  board1.size                                     //> res1: Int = 81
  val sudo1 = Sudoku.build(board1)                //> sudo1  : com.alaraph.sudoku.main.Sudoku = --- --- ---
                                                  //| --- --- ---
                                                  //| --- --- ---
                                                  //| 
                                                  //| --- --- ---
                                                  //| --- --- ---
                                                  //| --- --- ---
                                                  //| 
                                                  //| --- --- ---
                                                  //| --- --- ---
                                                  //| --- --- ---
                                                  //| 
                                                  //| 
  
  val s = SudokuSolver.solve(sudo1)               //> s  : Option[com.alaraph.sudoku.main.Sudoku] = Some(849 561 273
                                                  //| 561 273 849
                                                  //| 273 849 561
                                                  //| 
                                                  //| 485 916 732
                                                  //| 916 732 485
                                                  //| 732 485 916
                                                  //| 
                                                  //| 698 154 327
                                                  //| 154 327 698
                                                  //| 327 698 154
                                                  //| 
                                                  //| )
  
  val Some(z) = s                                 //> z  : com.alaraph.sudoku.main.Sudoku = 849 561 273
                                                  //| 561 273 849
                                                  //| 273 849 561
                                                  //| 
                                                  //| 485 916 732
                                                  //| 916 732 485
                                                  //| 732 485 916
                                                  //| 
                                                  //| 698 154 327
                                                  //| 154 327 698
                                                  //| 327 698 154
                                                  //| 
                                                  //| 
  z                                               //> res2: com.alaraph.sudoku.main.Sudoku = 849 561 273
                                                  //| 561 273 849
                                                  //| 273 849 561
                                                  //| 
                                                  //| 485 916 732
                                                  //| 916 732 485
                                                  //| 732 485 916
                                                  //| 
                                                  //| 698 154 327
                                                  //| 154 327 698
                                                  //| 327 698 154
                                                  //| 
                                                  //| 

}