package fpinscala.laziness

import Stream._

object Exercise {
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def fib: Stream[Int] = {
    def inner(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, inner(b, a+b))
    inner(0, 1)
  }

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some(a, ss) => cons(a, unfold(ss)(f))
      case None => empty
    }
  }

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a){Some(a, _)}

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n){m => Some(m, m+1)}

  def fibWithUnfold: Stream[Int] =
    unfold((0, 1)){(a, b) => Some(a, (b, a+b))}

  def map[A,B](as: Stream[A])(f: A => B): Stream[B] =
    unfold(as){s => s.headOption flatMap {a => Some(f(a), s.drop(1))}}

  def take[A](as: Stream[A], n: Int): Stream[A] =
    unfold((as, n)){(s, m) => s.headOption flatMap {a =>
                 if (m>0) Some(a, (s.drop(1), m-1)) else None}}

  def takeWhile[A](as: Stream[A])(p: A => Boolean): Stream[A] =
    unfold(as)(s => s.headOption flatMap {a =>
                 if(p(a)) Some(a, s.drop(1)) else None})

  def zipWith[A,B,C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((as, bs)){(asm, bsm) => asm.headOption.flatMap { a =>
                       bsm.headOption flatMap { b =>
                         Some(f(a, b), (asm.drop(1), bsm.drop(1)))
                       }}}

  def zipAll[A,B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((as, bs)){
        case (Empty, Empty) => None
        case (asm, bsm) => Some((asm.headOption, bsm.headOption), (asm.drop(1), bsm.drop(1)))
      }

  def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
    (s1, s2) match {
      case (_, Empty) => true
      case (Cons(h1, t1), Cons(h2, t2)) if(h1() == h2()) => startsWith(t1(), t2())
      case _ => false
    }

  def startsWithZip[A](s1: Stream[A], s2: Stream[A]): Boolean =
    !(zipAll(s1, s2).takeWhile((a, b) => a == b).isEmpty)

  def startsWithZipWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
    zipWith(s1, s2){_ == _}.forAll{_ == true}

  def tails[A](as: Stream[A]): Stream[Stream[A]] =
    unfold(as){
      case Cons(h, t) => Some(as, t())
      case Empty => None
    } append Stream(Empty)

  def hasSubsequence[A](sup: Stream[A], sub: Stream[A]): Boolean =
    tails(sup) exists (startsWith (_, sub))

  def scanRightNaive[A,B](as: Stream[A], z: B)(f: (A, => B) => B): Stream[B] =
    as match {
      case (Cons(h, t)) => Cons(() => as.foldRight(z)(f), () => scanRight(t(), z)(f))
      case Empty => Stream(z)
    }

  def scanRightWithUnfold[A,B](as: Stream[A], z: B)(f: (A, => B) => B): Stream[B] =
    unfold(as) { s =>
      s match {
        case (Cons(h, t)) => Some(s.foldRight(z)(f), t())
        case Empty => None
      }
    } append Stream(z)

  def scanRight[A,B](as: Stream[A], z: B)(f: (A, => B) => B): Stream[B] =
    as.foldRight((z, Stream(z))){(a, s) =>
      lazy val b = s
      val z1 = f(a, b._1)
      (z1, cons(z1, b._2))
    }._2
}
