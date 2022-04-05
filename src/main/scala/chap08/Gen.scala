package fpinscala.proptst

import fpinscala.stateful._
import fpinscala.laziness.Stream
import fpinscala.laziness.Exercise as e5

type SuccessCount = Int
type FailedCase = String
type TestCases = Int
type MaxSize = Int

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {(m, n, rng) =>
    run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case r => r
    }
  }

  def ||(p: Prop): Prop = Prop { (m, n, rng) =>
    run(m, n, rng) match {
      case Passed => Passed
      case Falsified(f1, s1) => p.run(m, n, rng)
    }
  }
}

object Prop {
  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    (m, n, rng) => randomStream(g)(rng).zip(e5.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    e5.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
    val casesPerSize = (n + max - 1) / max
    val props: Stream[Prop] = e5.from(0).take(n min max + 1).map {
      i => forAll(g(i))(f)
    }
    val prop: Prop = props.map {
      p => Prop {
        (max, _, rng) => p.run(max, casesPerSize, rng)
      }}.toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize(_))(f)

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap { n => Gen(State.sequence(List.fill(n)(sample))) }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val offset = stopExclusive - start
    Gen(State(Exercise.nonNegativeLessThan(offset))
          .map { _ + offset })
  }


  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    // choose(0, 10).flatMap { i => if(i > 4) g1 else g2 }
    boolean.flatMap { if(_) g1 else g2 }
  }

  def double: Gen[Double] = {
    Gen(State { Exercise.double(_) })
  }

  def weighted[A](a1: (Gen[A], Double), a2: (Gen[A], Double)): Gen[A] = {
    val (g1, d1) = a1
    val (g2, d2) = a2
    val d = d1 / (d1 + d2)
    double.flatMap(x => if(x < d) g1 else g2)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def unsized: SGen[A] = SGen(n => forSize(n))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(
    n => forSize(n).flatMap { f(_).forSize(n) }
  )

}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(
    n => Gen.listOfN(n, g)
  )
}
