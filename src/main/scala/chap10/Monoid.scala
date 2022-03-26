package fp.scala

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Test {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // exercise 10.1
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  // exercise 10.2
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) {

    }

    def zero = None
  }

  // exercise 10.3
  def endoMonoid[A] = new Monoid[A=>A] {
    def op(f: A=>A, g: A=>A) = f compose g
    def zero = identity
  }

  // exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // exercise 10.6
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A) = m.op(y, x)
    def zero = m.zero
  }

  def foldLeft[A, B](as: List[A], z: B)(f:(B, A) => B): B = {
    val m = new Monoid[B => B] {
      def op(f: B => B, g: B => B) = g compose f
      def zero = identity
    }
    foldMap(as, m)(a => b => f(b, a))(z)
  }

  def foldRight[A, B](as: List[A], z: B)(f:(A, B) => B): B = {
    val m = new Monoid[B => B] {
      def op(f: B => B, g: B => B) = f compose g
      def zero = identity
    }
    foldMap(as, m)(a => b => f(a, b))(z)
  }

  def foldLeft1[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldRight1[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(endoMonoid[B])(f.curried)(z)

  // exercise 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.length match {
      case 0 => m.zero
      case 1 => f(v(0))
      case _ => {
        val (v1, v2) = v.splitAt(v.length / 2)
        m.op(foldMapV(v1, m)(f), foldMapV(v2, m)(f))
      }
    }
  }
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object Exercise10_10 {
  val wcMonoid = new Monoid[WC] {
    def split(chars: String): WC = {
      chars.length match {
        case 0 | 1 => Stub(chars)
        case _ => {
          val m = chars.length / 2
          op(split(chars.substring(0, m)), split(chars.substring(m)))
        }
      }
    }

    def processNonAlpha(left: String, right: String): (String, String, Boolean) = {
      val ll = left.length
      val nonAlphaTail = ll > 0 && !left(ll-1).isLetter
      val processedLeft = if (nonAlphaTail) left.slice(0, ll - 1)
                          else left
      val nonAlphaHead = right.length > 0 && !right(0).isLetter
      val processedRight = if(nonAlphaHead) right.substring(1) else right
      (processedLeft, processedRight, nonAlphaTail || nonAlphaHead)
    }

    def processTail(s: String): (String, Boolean) = {
      val len = s.length
      val nonAlphaTail = len > 0 && !s(len-1).isLetter
      val processed = if (nonAlphaTail) s.slice(0, len - 1)
                      else s
      (processed, nonAlphaTail)
    }

    def processHead(s: String): (String, Boolean) = {
      val nonAlphaHead = !s.isEmpty && !s(0).isLetter
      val processed = if(nonAlphaHead) s.substring(1)
                      else s
      (processed, nonAlphaHead)
    }

    def op(left: WC, right: WC): WC = {
      (left, right) match {
        // case (Stub(""), _) => right
        // case (_, Stub("")) => left
        case (Stub(l), Stub(r)) => {
          val (l1, r1, nonAlpha) = processNonAlpha(l, r)
          if(nonAlpha)
            Part(l1, 0, r1)
          else
            Stub(l + r)
        }
        case (Stub(l), Part(rl, n, rr)) => {
          val (l1, _, nonAlpha) = processNonAlpha(l, rl)
          if(nonAlpha)
            Part(l1, n+1, rr)
          else
            Part(l+rl, n, rr)
        }
        case (Part(l, n, r), Stub(rr)) => {
          val (_, rr1, nonAlpha) = processNonAlpha(r, rr)
          if(nonAlpha)
            Part(l, n+1, rr1)
          else
            Part(l, n, r+rr)
        }
        case (Part(l1, n1, r1), Part(l2, n2, r2)) =>
          val (_, _, nonAlpha) = processNonAlpha(r1, l2)
          val n = if(nonAlpha) n1 + n2
                  else n1 + n2 + 1
          Part(l1, n, r2)
      }
    }
    def zero = Stub("")
  }

  // exercise 10.11
  def countWords(words: String): Int = {
    val wc = wcMonoid.split(words)
    println(wc)
    wc match {
      case Stub("") => 0
      case Stub(_) => 1
      case Part(l, n, r) => {
        var m = n
        if (l.length > 0) m += 1
        if (r.length > 0) m += 1
        m
      }
    }
  }
}

object Exer10_10 {
  def unstub(s: String) = s.length min 1

  val wcMonoid = new Monoid[WC] {
    def op(left: WC, right: WC): WC = {
      (left, right) match {
        case (Stub(l), Stub(r)) => Stub(l + r)
        case (Stub(ll), Part(l, n ,r)) => Part(ll + l, n, r)
        case (Part(l, n, r), Stub(rr)) => Part(l, n, r + rr)
        case (Part(l1, n1, r1), Part(l2, n2, r2)) =>
          Part(l1, n1 + n2 + unstub(r1+l2), r2)
      }
    }

    def zero = Stub("")
  }

  def count(words: String): Int = {
    def wc(c: Char): WC =
      if(c.isLetter) Stub(c.toString)
      else Part("", 0, "")

    val m = foldMapV(words.toIndexedSeq, wcMonoid)(wc)
    println(m)
    m match {
      case Stub(s) => unstub(s)
      case Part(l, n, r) => unstub(l) + n + unstub(r)
    }
  }
}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}

