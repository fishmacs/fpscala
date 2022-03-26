object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Prop {
  import Prop._

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(p: Prop): Prop = new Prop {
    def check: Either[(FailedCase, SuccessCount), SuccessCount] = check match {
      case Left(f, s) => 
    }
  }
}

class Gen[A] {
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}
