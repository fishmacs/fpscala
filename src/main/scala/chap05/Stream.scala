package fpinscala.laziness

sealed trait Stream[+A] {
  import Stream._

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

  def toList: List[A] = this match
  case Empty => Nil
  case Cons(h, t) => h() :: t().toList

  def headOption: Option[A] = this match
  case Empty => None
  case Cons(h, t) => Some(h())

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    // make sure tail is not evalued while n == 1
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match
  case Cons(_, t) if(n > 0) => t().drop(n-1)
  case _ => this

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case Empty => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

  def existsWithFold(p: A => Boolean): Boolean =
    foldRight(false){p(_) || _}

  def forAll(p: A => Boolean): Boolean =
    foldRight(true){p(_) && _}

  def takeWhileWithFold(p: A => Boolean): Stream[A] =
    foldRight(empty){(a, b) => if(p(a)) cons(a, b) else empty}

  def headOptionWithFold: Option[A] =
    foldRight(None: Option[A]){(h, _) => Some(h)}

  def map[B](f: A => B): Stream[B] =
    foldRight(empty){(a, b) => cons(f(a), b)}

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty){(a, b) => if(p(a)) cons(a, b) else b}

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s){cons}

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty){f(_).append(_)}

  def zip1[B](s: => Stream[B]): Stream[(A, B)] =
    Exercise.zipWith(this, s){(_, _)}

  def zip[B](bs: Stream[B]): Stream[(A,B)] = (this, bs) match {
    case (Cons(a, at), Cons(b, bt)) => cons((a(), b()), at().zip(bt()))
    case _ => Empty
  }

  def find(p: A => Boolean): Option[A] = this match {
    case Cons(a, as) => {
      val v = a()
      if (p(v)) Some(v)
      else as().find(p)
    }
    case _ => None
  }

  def find1(p: A => Boolean): Option[A] =
    filter(p).headOption
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
