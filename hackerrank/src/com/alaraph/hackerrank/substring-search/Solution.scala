/**
 *   Copyright 2022 www.alaraph.com
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
import scala.io.StdIn.readLine

object Solution {

  def KMP(word: String, sstring: String, first_only: Boolean = true): List[Int] = {
    val w = word.toVector
    val s = sstring.toVector

    def KMP_table(cnd: Int = 0, pos: Int = 1, acc_t: Vector[Int] = Vector(-1)): Vector[Int] = {
      def next_cnd(n: Int):Int =
        if (n >= 0 && w(pos) != w(n)) next_cnd(acc_t(n))
        else n+1

      if (pos < w.size) {
        if (w(pos) == w(cnd))
          KMP_table(cnd+1, pos+1, acc_t :+ acc_t(cnd))
        else
          KMP_table(next_cnd(cnd), pos+1, acc_t :+ cnd)
      } else acc_t :+ cnd
    }

    val t = KMP_table()

    def search(p: List[Int] = Nil, iw: Int = 0, is: Int = 0): List[Int] = {
      if (is < s.length) {
        if (w(iw) == s(is)) {
          if (iw == w.length - 1) {
            if (first_only) List(is-iw)
            else search((is-iw)::p, t(iw+1), is+1)
          } else search(p, iw+1, is+1)
        } else {
          val _iw = t(iw)
          if (_iw < 0) search(p, _iw+1, is+1)
          else search(p, _iw, is)
        }
      } else p.reverse
    }
    search()
  }

  def main(args: Array[String]): Unit = {
    val n = readLine.toInt
    1 to n foreach { _ =>
      val sstr = readLine
      val word = readLine
      KMP(word, sstr) match {
        case Nil => println("NO")
        case _ => println("YES")
      }
    }
  }

}
