package fpinscala.errorhandling

object Exercise {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map((x: Double) => math.pow(x - m, 2))))

  // Exercise 4.3
  def map2a[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def map2WithComprehension[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::tl => h flatMap (x => sequence(tl) map (x::_))
    // case None::_ => None
    // case Some(x)::oas => sequence(oas) match {
    //   case None => None
    //   case Some(la) => Some(x::la)
    // }
  }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil))((oa: Option[A], acc: Option[List[A]]) => map2(oa, acc)(_ :: _))

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil))((x: A, acc: Option[List[B]]) =>
      map2(f(x), acc)(_ :: _))

  def sequenceWithTranverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}

object ExerciseEither {
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    as.foldRight(Right(Nil))((x, acc: Either[E, List[B]]) => f(x).map2(acc){_ :: _})

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)
}
