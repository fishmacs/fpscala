object Exercise {
  // exer2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fib1(n: Int, acc1: Int, acc2: Int): Int =
      n match {
        case 0 => acc1
        case _ => fib1(n-1, acc2, acc1 + acc2)
      }
    fib1(n, 0, 1)
  }

  // exer2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    as.length match {
      case 0 | 1 => true
      case 2 => ordered(as(0), as(1))
      case _ => {
        val (l, r) = as.splitAt(as.length / 2)
        isSorted(l, ordered) && isSorted(l, ordered)
      }
    }
  }

  def isSorted1[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def inner(index: Int): Boolean = 
      if (as.length <= index + 1) true
      else {
        if(ordered(as(index), as(index + 1)))
          inner(index + 1)
        else false
      }

    inner(0)
  }

  // exer2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  // exer2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // exer2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
