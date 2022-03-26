package fpinscala.stateful

case class State[S, +A](run: S => (A, S)) {
  import State.unit

  def flatMap[B](g: A => State[S,B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    g(a).run(s1)
  })

  def map[B](g: A => B): State[S, B] =
    flatMap {a => unit(g(a))}

}

object State {
  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def map2[S,A,B,C](f: State[S, A], g: State[S, B])(h: (A, B) => C): State[S, C] =
    f.flatMap { a => g.map { b => h(a, b) } }

  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit(List[A]())) { (f, g) => map2(f, g){_ :: _} }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
