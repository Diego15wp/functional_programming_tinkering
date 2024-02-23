object SieveEratosthenes {

  def go = {
    val n = 30
    val r = allPrimes(n)
    println(s"All primes smaller than ${n}: ${r}")
  }

  def allPrimes(n: Int): List[Int] = ???

}
