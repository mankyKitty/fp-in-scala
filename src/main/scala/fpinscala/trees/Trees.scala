package fpinscala.trees

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeMod {

  def size[A](t:Tree[A]): Int =
    treeFold(t,0)( (_,n) => n + 1)

  def maximum(t:Tree[Int]): Int =
    treeFold(t,0)(_ max _)

  def depth[A](t:Tree[A]): Int = {

    def go[A](ts:Tree[A], n:Int): Int = ts match {
      case Leaf(_) => n + 1
      case Branch(l,r) => (n + 1) + ((go(l,0)) max (go(r,0)))
    }

    go(t,0)
  }

  def treeMap[A,B](t:Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(treeMap(l)(f),treeMap(r)(f))
  }

  def treeFold[A,B](t:Tree[A], z: B)(f: (A,B) => B): B = t match {
    case Leaf(v) => f(v,z)
    case Branch(l,r) => treeFold(l,treeFold(r,z)(f))(f)
  }
}

