package fpinscala.stream

import Stream._

sealed trait Stream[+A] {

  def headOption: Option[A] =
    this.foldRight[Option[A]](None)( (a,_) => Some(a) )

  def toList: List[A] =
    this.foldRight[List[A]](List())( _ :: _ )

  def take(n: Int): Stream[A] =
    if (n < 0) Empty
    else
      unfold((this,n))(
        _ match {
          case (Empty,_) => None
          case (_, 0) => None
          case (Cons(h,t), nn) => Some((h(), (t(), nn - 1)))
        }
      )

  // {
  //   def go(s: Stream[A], x: Int): Stream[A] = (s,x) match {
  //       case (Empty,_) => Empty
  //       case (Cons(h,t), 0) => Cons(h, () => Empty)
  //       case (Cons(h,t), nn) => Cons(h, () => t().take(nn - 1))
  //     }

  //   if (n > 0) go(this,n) else Empty
  // }

  def drop(n: Int): Stream[A] = {
    def go(s: Stream[A], x: Int): Stream[A] = (s,x) match {
      case (Empty,_) => Empty
      case (t,0) => t
      case (Cons(_,t),nn) => go(t(), nn - 1)
    }

    if (n < 0) Empty else go(this,n)
  }

  def map[B](f: A => B): Stream[B] =
    //this.foldRight[Stream[B]](Empty)( (a,b) => cons(f(a), b) )
    unfold(this)( s => s.headOption.map( a => (f(a), s.drop(1))) )

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty)(
      (a,b) => if (p(a)) cons(a,b) else b
    )

  def append[B >: A](s: => Stream[B]) =
    this.foldRight[Stream[B]](s)( cons(_,_) )

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight[Stream[B]](Empty)(
      (a,b) => f(a).append(b)
    )

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this)(
      _ match {
        case Empty => None
        case Cons(h,t) =>
          if (p(h())) Some((h(),t())) else None
      })
    // this.foldRight[Stream[A]](Empty)(
    //   (a,b) => if (p(a)) cons(a, b.takeWhile(p)) else Empty
    // )

  def startsWith[A](s: Stream[A]): Boolean =
    this.zipWith(s)( _ == _ ).forAll( _ == true )

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    unfold(this.tails)(
      _ match {
        case Empty => None
        case Cons(h,t) => Some(
          (h().foldRight(z)(f), t())
        )
      }
    ) append cons(z,empty)

  def tails: Stream[Stream[A]] =
    unfold(this)(
      _ match {
        case Empty => None
        case Cons(h,t) => Some( (Cons(h,t), t()) )
      }
    ) append Empty

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C ): Stream[C] =
    unfold((this,s2))( s =>
      for {
        hA <- s._1.headOption
        hB <- s._2.headOption
      } yield (f(hA,hB), (s._1.drop(1), s._2.drop(1)))
    )

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this,s2))(
      s => s match {
        case (Empty,Empty) => None
        case (a,b) => Some(
          (a.headOption,b.headOption),
          (a drop 1, b drop 1)
        )
      }
    )

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    this.foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => t().foldRight(p(h()))((a,b) => p(a) && b)
    case _ => false
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  val ones: Stream[Int] = constant(1)

  def cons[A](hd: A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a,s)) => cons(a, unfold(s)(f))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def from(n: Int): Stream[Int] =
    unfold(n)( x => Some(x, x + 1) )

  def fibs(): Stream[Int] = {
    def fib(n: Int): Int = {
      if (n == 0 || n == 1) n
      else fib(n - 1) + fib(n - 2)
    }

    from(0).map(fib)
  }

  def constant[A](a: A): Stream[A] =
    unfold(a)( x => Some((a,a)))
}
