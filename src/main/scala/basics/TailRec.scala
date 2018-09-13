package basics

class TailRec {


  def listValueAverage(list: Array[Int]): Float = {
    def accumulator(l: List[Int], acc: Int = 0): Float = l match {
      case x :: xs => accumulator(xs, acc + x)
      case _ => acc
    }

    accumulator(list.toList) / list.length
  }

  def concatenateStrings(m: String, r: Int): String = {
    def dp(m: String, r: Int, acc: String): String = {
      if (r == 0) acc
      else dp(m, r - 1, acc + m)
    }

    dp(m, r, "")
  }

  def factorialTailRec(n: Int): Int = {
    def accumulator(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else accumulator(n - 1, acc * n)
    }

    accumulator(n, 1)
  }

  def isPrimeTailRec(n: Int): Boolean = {
    def stillPrime(t: Int, isStillPrime: Boolean): Boolean = {
      if (!isStillPrime) false
      else if (t <= 1) true
      else stillPrime(t - 1, n % t != 0 && isStillPrime)
    }

    stillPrime(n / 2, true)
  }

  def fibTailRec(n: Int): Int = {
    def accumulator(x: Int, last: Int, nextToLast: Int): Int = {
      if (x >= n) last
      else accumulator(x + 1, last + nextToLast, last)
    }

    if (n <= 2) 1
    else accumulator(3, 1, 1)
  }
}