package fpinscala.datastructures

object Exercise3 {
  // exer 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tl) => tl
  }

  // exer 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A]  = l match {
    case Nil => Nil
    case Cons(_, tl) => drop(tl, n - 1)
  }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(x, xs) if p(x) => dropWhile(xs, p)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil  => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // exer 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    def inner(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(y, ys) => inner(ys, f(acc, y))
    }
    inner(as, z)
  }

  // exer 3.11
  def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

  // exer 3.12
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((xs: List[A], a: A) => Cons(a, xs))

  // exer 3.13
  def foldLeft1[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    List.foldRight(reverse(as), z)((a, b) => f(b, a))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    List.foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // exer 3.14
  def append[A](l1: List[A], l2: List[A]): List[A] =
    List.foldRight(l1, l2)(Cons(_, _))

  // exer 3.15
  def flatConcat[A](ll: List[List[A]]): List[A] = {
    // def revAppend(l1: List[A], l2: List[A]): List[A] =
    //   foldLeft(l1, l2)((l, a) => Cons(a, l))

    ll match {
      case Nil => Nil
      case Cons(as, ass) => append(as, flatConcat(ass))
    }
  }

  def flatConcat1[A](ll: List[List[A]]): List[A] =
    List.foldRight(ll, Nil: List[A])(append)

  // exer 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = 
    as match {
      case Nil => Nil
      case Cons(h, tl) => Cons(f(h), map(tl)(f))
    }

  def mapWithFold[A,B](as: List[A])(f: A => B): List[B] =
    List.foldRight(as, Nil: List[B])((a, tl) => Cons(f(a), tl))

  // exer 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(h, tl) => if (f(h)) Cons(h, filter(tl)(f))
                          else filter(tl)(f)
    }

  def filterWithFold[A](as: List[A])(f: A => Boolean): List[A] =
    List.foldRight(as, Nil: List[A])((h, tl) => if(f(h)) Cons(h, tl) else tl)

  // exer 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatConcat1(map(as)(f))

  // exer 3.21
  def filter1[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) Cons(a, Nil) else Nil)

  // exer 3.23
  def zipWith[A,B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(a, ta), Cons(b, tb)) => Cons(f(a, b), zipWith(ta, tb)(f))
    }

  // exer 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def contains(l1: List[A], l2: List[A]): Boolean =
      (l1, l2) match {
        case (_, Nil) => true
        // case (Nil, Cons(_, _)) => false
        case (Cons(h1, t1), Cons(h2, t2)) => if(h1 == h2) contains(t1, t2)
                                             else contains(t1, sub)
        case _ => false
    }
    contains(sup, sub)
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) if(h1 == h2) => startsWith(t1, t2)
    case _ => false
  }

  def hasSubsequenceWithStartsWith[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequenceWithStartsWith(t, sub)
    }
}
