package fpinscala.stateful

import fpinscala.stateful.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Exercise611 {
  def work(input: Input)(m: Machine): Machine =
    (m, input) match {
      case (Machine(true, candies, coins), Coin) if (candies > 0) =>
        Machine(false, candies, coins + 1)
      case (Machine(false, candies, coins), Turn) =>
        Machine(true, candies - 1, coins)
      case _ => m
    }

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map { modify[Machine] compose work} )
    m <- get
  } yield (m.candies, m.coins)
}
