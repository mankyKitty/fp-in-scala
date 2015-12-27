package fpinscala.option
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

  def option[B](f: A => B)(g: => B) = this match {
    case None => g
    case Some(a) => f(a)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object OptionCompanion {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m , 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a.flatMap( aPrime => b.map( bPrime => f(aPrime,bPrime) ))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)( x => x )

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))(
      (h,t) => t.flatMap( tt => f(h).map( _ :: tt ))
    )
}
