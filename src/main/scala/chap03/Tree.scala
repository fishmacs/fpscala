sealed trait Tree[+A] {
  // exercise 3.25
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  def depth: Int = this match {
    case Leaf(_) => 0
    case Branch(l, r) => r.depth max l.depth + 1
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def fold[B](f: A => B)(g: (B, B) => B): B = this match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(l.fold(f)(g), r.fold(f)(g))
  }

  def sizeViaFold: Int = fold{_ => 1}{_ + _}

  def depthViaFold: Int = fold{ _ => 0 }{ _ max _ }

  def mapViaFold[B](f: A => B): Tree[B] =
    fold{a => Leaf(f(a)): Tree[B]}{Branch(_, _)}
}

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object ExerciseTree {
  // exercise 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def maximumViaFold(tree: Tree[Int]): Int = tree.fold(a => a)(_ max _)
}
