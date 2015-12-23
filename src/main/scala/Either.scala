import scala.{Option => _, Either => _, Left => _, Right => _, _}

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE, C] =
    this.flatMap( aa => b.map( bb => f(aa,bb) ) )
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object EitherCompanion {
  def sequence[E, A](es: List[Either[E,A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil))(
      (h, t) => h.map2(t)( _ :: _ )
    )

}
