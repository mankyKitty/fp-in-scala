// Hide the default implementations
import scala.{Option => _, Some => _, Either => _,_}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    ob.map(a => this.getOrElse(a))

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if (f(a)) Some(a) else None)

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m , 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap( aPrime => b.flatMap( bPrime => Some( f(aPrime,bPrime) )))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => None
    case x::xs => xs.foldRight(x.map( _ :: List()))( map2(_,_)( _ :: _ ) )
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

