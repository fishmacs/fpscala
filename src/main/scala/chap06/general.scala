package fpinscala.stateful

object GeneralState {
  // case class State[S, +A](run: S => (A, S))
  type State[S,+A] = S => (A, S)
  type Rand[A] = State[RNG, A]

  // Exercise 6.10
  def unit[S,A](a: A): State[S,A] = s => (a, s)

  def flatMap[S,A,B](f: State[S,A])(g: A => State[S,B]): State[S, B] = s => {
    val (a, s1) = f(s)
    g(a)(s1)
  }

  def map[S,A,B](f: State[S,A])(g: A => B): State[S, B] =
    flatMap(f){a => unit(g(a))}

  def map2[S,A,B,C](f: State[S,A], g: State[S,B])(h: (A, B) => C): State[S, C] =
    flatMap(f){a => map(g){b => h(a, b)}}

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit(List[A]())){(f, g) => map2(f, g){_ :: _}}
}
