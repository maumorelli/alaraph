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
 * This is my solution to problem:
 * https://www.hackerrank.com/challenges/fp-reverse-a-list
 * mauro@alaraph.com
 */
package com.alaraph.hackerrank.revlist

object Solution extends App {
def f(arr:List[Int]):List[Int] = {
    def fAcc(arr:List[Int], acc:List[Int]):List[Int] = arr match {
            case Nil => acc
            case h::tail => fAcc(tail, h::acc)
    }
    fAcc(arr, Nil)
}

println(f(io.Source.stdin.getLines.toList.map(_.trim).map(_.toInt)).mkString("\n"))
}
