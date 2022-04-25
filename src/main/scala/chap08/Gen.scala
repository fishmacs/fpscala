import fpinscala.stateful.State
import fpinscala.stateful.RNG
import fpinscala.stateful.SimpleRNG
import fpinscala.stateful.Exercise
import fpinscala.laziness.Stream
import fpinscala.laziness.Exercise as E5
import Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {(m, n, r) =>
    run(m, n, r) match {
      case Passed | Proved => p.run(m, n, r)
      case f => f
    }
  }

  def ||(p: Prop): Prop = Prop {(m, n, r) =>
    run(m, n, r) match {
      case Falsified(_, _) => p.run(m, n, r)
      case p => p
    }}
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {(_, n, r) =>
    randomStream(a)(r).zip(E5.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed
        else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](a: SGen[A])(f: A => Boolean): Prop =
    forAll(a(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
    Prop { (maxSize, testCases, rng) =>
      val casesPerSize = (testCases + maxSize - 1) / maxSize
      val prop = E5.from(0)
        .take((testCases min maxSize) + 1)
        .map{i => forAll(g(i))(f)}
        .map{p => Prop {(max, _, r) => p.run(max, casesPerSize, r)}}
        .toList
        .reduce(_ && _)
      prop.run(maxSize, testCases, rng)
    }

  def check(p: => Boolean): Prop = Prop {(_, _, _) =>
    if (p) Proved
    else Falsified("()", 0)
  }

  def randomStream[A](g: Gen[A])(r: RNG): Stream[A] =
    E5.unfold(r){rng => Some(g.sample.run(rng))}

  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\ngenerated an exception: ${e.getMessage}\nstack trace:\n${e.getStackTrace.mkString("\n")}"

  def run(prop: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100,
          rng: RNG =SimpleRNG(System.currentTimeMillis)): Unit =
    prop.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ Ok, passed $testCases tests.")
      case Proved =>
        println("+ Ok, proved property")
    }
}

case class Gen[+A](sample: State[RNG, A]) {
  def unsized: SGen[A] = SGen(_ => this)
  
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen{
    sample.flatMap(f(_).sample)
  }

  def map[B](f: A => B): Gen[B] = Gen {
    sample.map(f)
  }

  def map2[B,C](gb: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- this
    b <- gb
  } yield f(a, b)

  def map2a[B,C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    flatMap(a => gb.map(b => f(a, b)))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val offset = stopExclusive - start
    val state = State(Exercise.nonNegativeLessThan(offset))
      .map { _ + offset }
    Gen(state)
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(
    State(RNG.int) map {_ > 0}
  )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(
    State {rng => (E5.unfold(rng)(r => Some(g.sample.run(r))).take(n).toList, rng)}
  )

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
      Gen.listOfN(_, g)
    }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen { i =>
    Gen.listOfN(i max 1, g)
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { n => forSize(n).flatMap(f(_).forSize(n))}

  def map[B](f: A => B): SGen[B] = SGen { n => forSize(n).map(f) }

  def apply(size: Int): Gen[A] = forSize(size)
}

object SGen {
  def unit[A](a: => A): SGen[A] = SGen { n => Gen.unit(a) }

}
