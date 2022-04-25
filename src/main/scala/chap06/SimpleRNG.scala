package fpinscala.stateful

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def boolean: Rand[Boolean] = map(int) { _ % 2 == 0 }
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dl + 0xbl) & 0xffffffffffffl
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Exercise {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (i & 0x7fffffff, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r1) = double(r)
    ((i, d), r1)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r1) = r.nextInt
    ((d, i), r1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d1, r1) = double(r)
    val (d2, r2) = double(r1)
    ((d, d1, d2), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def inner(n: Int, acc: List[Int], r: RNG): (List[Int], RNG) =
      if (n == 0) (acc, rng)
      else {
        val(i, r1) = r.nextInt
        inner(n-1, i::acc, r1)
      }
    inner(count, Nil, rng)
  }

  // Exercise 6.5
  // import RNG
  import RNG.Rand

  def doubleWithMap: Rand[Double] = {
    RNG.map(nonNegativeInt){_.toDouble / (Int.MaxValue.toDouble + 1)}
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  // Exercise 6.6
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(RNG.int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, RNG.int)

  // Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List[A](), rng)) {
      case (rand, (as, r)) => {
        val (a, r1) = rand(r)
        (a :: as, r1)
      }
    }
  }

  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight{RNG.unit(List[A]())}{(r1, r0) => map2(r1, r0){_ :: _}}

  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(RNG.int))

  // Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + n - 1 - mod >= 0) RNG.unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  // Exercise 6.9
  def map[A,B](f: Rand[A])(g: A => B): Rand[B] =
    flatMap(f){a => RNG.unit(g(a))}

  def map2WithFlatMap[A,B,C](f: Rand[A], g: Rand[B])(h: (A, B) => C): Rand[C] =
    flatMap(f){a => map(g){b => h(a, b)}}

}
