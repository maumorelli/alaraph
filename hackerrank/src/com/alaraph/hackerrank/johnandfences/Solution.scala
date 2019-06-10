object Solution {
 
  import scala.io.StdIn.readLine

  def g(l:List[Int], h:Int, len: Int):Int = l match {
    case x::xx => if (x >= h) g(xx,h,len+1) else h*len
    case Nil => h*len
  }
    
  def f(ll:List[Int], rl:List[Int], acc: Int):Int = rl match {
    case x::xx => f(x::ll, xx, math.max(acc, g(xx,x,1)+g(ll,x,0)))
    case Nil => acc 
  }  
   
  def main(args: Array[String]): Unit = {
    val n = readLine.toInt
    val l = readLine.split(' ').map(_.toInt).toList
    val res = f(Nil, l, 0)
    println(res)
  }
}
