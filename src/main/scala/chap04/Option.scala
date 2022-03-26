package fpinscala.errorhandling

sealed trait Option[+A] {
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if(f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def list[A,B](f: A => B): Option[A] => Option[B] = _ map f
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }
}

// object Example {
//   def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double
//   def pareseInsurenaceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
//     val optAge = Try(age.toInt)
//     val optTickets = Try(numberOfSpeedingTickets.toInt)
//     map2(optAge, optTickets)(insuranceRateQuote)
//   }
// }
