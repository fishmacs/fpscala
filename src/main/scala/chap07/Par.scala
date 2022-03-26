package fpinscala.parallelism

import java.util.concurrent.ExecutorService
// import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch

sealed trait Future[+A] {
  private[parallelism]
  def apply(k: A => Unit): Unit
}

object Par {
  type Par[+A] = ExecutorService => Future[A]

  // private case class UnitFuture[A](get: A) extends Future[A] {
  //   def isDone = true
  //   def get(timeout: Long, units: TimeUnit) = get
  //   def isCancelled = false
  //   def cancel(evenIfRunning: Boolean): Boolean = false
  // }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def map[A,B](p: Par[A])(f: A => B): Par[B] = es => new Future[B] {
    def apply(cb: B => Unit): Unit = {
      p(es){a => eval(es){cb(f(a))}}
    }
  }

  def mapWithMap2[A,B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(())){(x, _) => f(x)}

  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => new Future[C] {
    def apply(cb: C => Unit): Unit = {
      var ar: Option[A] = None
      var br: Option[B] = None

      val combiner = Actor[Either[A,B]](es) {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(cb(f(a, b)))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(cb(f(a, b)))
        }
      }

      p(es)(a => combiner ! Left(a))
      p2(es)(b => combiner ! Right(b))
    }
  }

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C)=>D): Par[D] =
    map2(a, map2(b, c) { (b1: B, c1: C) => (b1, c1) }){
      case (x, (y, z)) => f(x, y, z)
    }

  def fork[A](a: => Par[A]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit =
      eval(es)(a(es)(cb))
  }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {def call = r})

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = a =>
    lazyUnit((f(a)))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]())){map2(_,_){_::_}}

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(fork(sequenceBalanced(l)), fork(sequenceBalanced(r)))(_ ++ _)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    asyncF{ (xs: List[A]) => xs.filter(f) }(as)

  def parFilter1[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pll = as.map(asyncF{ a => if (f(a)) List(a) else List() })
    map(sequence(pll)){_.flatten}
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => 
    new Future {
      def apply(cb: A => Unit): Unit =
        cond(es) {
          if (_) eval(es){t(es)(cb)}
          else eval(es){f(es)(cb)}
        }
    }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es =>
    new Future {
      def apply(cb: A => Unit): Unit =
        n(es) { i => eval(es){choices(i)(es)(cb)} }
    }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond){ if (_) 1 else 0 })(List(t, f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es =>
    new Future {
      def apply(cb: V => Unit): Unit =
        key(es) { k => eval(es){choices(k)(es)(cb)} }
    }

  def flatMap[A,R](arg: Par[A])(f: A => Par[R]): Par[R] = es => {
    new Future {
      def apply(cb: R => Unit): Unit =
        arg(es){a => eval(es){f(a)(es)(cb)}}
    }
  }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond){if (_) t else f}

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n){choices(_)}

  def choiceMapViaChooser[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key){choices(_)}

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    new Future {
      def apply(cb: A => Unit): Unit =
        a(es){p => eval(es){p(es)(cb)}}
    }
  }

  def flatMapViaJoin[A,B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(identity)
}

object Example {
  import Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size < 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r))){ _ + _ }
    }

  def summation[A,B](xs: IndexedSeq[A], z: B)(f: (A, B) => B)(g: (B, B) => B): Par[B] =
    xs.size match {
      case 0 => Par.unit(z)
      case 1 => Par.unit(f(xs(0), z))
      case _ => {
        val (l, r) = xs.splitAt(xs.size / 2)
        val lr = fork(summation(l, z)(f)(g))
        val rr = fork(summation(r, z)(f)(g))
        map2(lr, rr)(g)
      }
    }

  def countWords(paragragh: List[String]): Par[Int] =
    summation(paragragh.toIndexedSeq, 0)((s: String, _: Int) => s.split(" ").length)((a: Int, b: Int) => a + b)
    
}
