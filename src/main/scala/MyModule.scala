// A comment !
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n,1)
  }

  def fib(n: Int): Int = {
    if (n == 0 || n == 1) n
    else fib(n - 1) + fib(n - 2)
  }

  private def formatAbs(x : Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x ,abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(n, f(n))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def comp(a:Int,b:Int):Boolean = a <= b

  def isSorted[A](as:Array[A], c: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n:Int): Boolean =
      if (n == (as.length - 1)) true
      else c(as(n),as(n+1)) && go(n+1)

    go(0)
  }

  def partial1[A,B,C](a:A, f: (A,B) => C): B => C =
    (b: B) => f(a,b)

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    (a:A) => (b:B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a:A,b:B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a:A) => f(g(a))

  def setHead[A](a:A, as:List[A]): List[A] = as match {
    case Nil => List(a)
    case _::ys => a::ys
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil   => Nil
      case _::ys =>
        if (n <= 0) l else drop(ys, n - 1)
    }

  def tail[A](xs:List[A]): List[A] = drop(xs,1)

  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case xs@(x::ys) =>
        if (f(x)) dropWhile(ys)(f) else xs
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _::Nil => Nil
    case x::xs => x :: init(xs)
  }

  def foldRight[A,B](as:List[A],z:B)(f: (A,B) => B): B =
    foldLeft(as.reverse,z)((b,a) => f(a,b))

  @annotation.tailrec
  def foldLeft[A,B](as:List[A],z:B)(f: (B,A) => B): B = as match {
    case Nil => z
    case x::xs => foldLeft(xs,f(z,x))(f)
  }

  def sum(l: List[Int]): Int =
    foldLeft(l,0)(_+_)

  def product(ns: List[Double]): Double =
    foldLeft(ns,1.0)(_*_)

  def length[A](l:List[A]): Int =
    foldLeft(l,0)((n,_) => n + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l,List[A]())((b,a) => a::b)

  def append[A](xs:List[A], ys:List[A]): List[A] =
    foldRight(xs,ys)(_::_)

  def concat[A](xxs:List[List[A]]): List[A] =
    foldLeft(xxs,List[A]())(MyModule.append(_,_))

  def oneToAll(xs:List[Int]): List[Int] =
    foldRight(xs,List[Int]())( _ + 1 :: _)

  def stringlyDs(ds:List[Double]): List[String] =
    foldRight(ds,List[String]())(_.toString :: _)

  def map[A,B](as:List[A])(f: A => B): List[B] =
    foldRight(as,List[B]())(f(_) :: _)

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as,List[A]())((a,b) =>
      if (f(a)) (a::b) else b
    )

  def flatMap[A,B](as:List[A])(f: A => List[B]): List[B] =
    foldLeft(as,List[B]())((b,a) => append(b,f(a)))

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a) => if (f(a)) List(a) else List())

  def addLists(a1: List[Int], a2: List[Int]): List[Int] = {

    @annotation.tailrec
    def go(l1:List[Int],l2:List[Int],acc:List[Int]): List[Int] = l1 match {
      case Nil => acc
      case x::xs => l2 match {
        case Nil => acc
        case y::ys => go(xs,ys, ((x:Int) + (y:Int)::acc))
      }
    }

    reverse(go(a1,a2,List[Int]()))
  }

  def zipWith[A,B,C](a1:List[A],a2:List[B])(f:(A,B) => C): List[C] = {
    @annotation.tailrec
    def go(l1:List[A],l2:List[B],acc:List[C]): List[C] = l1 match {
      case Nil => acc
      case x::xs => l2 match {
        case Nil => acc
        case y::ys => go(xs,ys,(f(x,y)::acc))
      }
    }
    reverse(go(a1,a2,List[C]()))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => false
    case x::_ => {
      val a = flatMap(sup)((b) =>
        if (x == b) drop(sup,sup.indexOf(b)).take(sub.length)
        else List()
        )
      a == sub
    }
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
}
