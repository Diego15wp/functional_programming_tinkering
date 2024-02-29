object SieveEratosthenes {

  def go = {
    val n = 30
    val r = allPrimes(n)
    println(s"All primes smaller than ${n}: ${r}")
  }

  def allPrimes(n: Int): List[Int] = {
    //inner "loop"/recursive function to create new list of primes
    @annotation.tailrec
    def primes(newlist: List[Int], res: List[Int]): List[Int] = {
      if(newlist.isEmpty) res.reverse
      else {
        val new_prime = newlist.head  //head of res will be
        primes(newlist.filter(x => x % new_prime != 0), new_prime :: res)
      }
    }

    //call to recursive function with full range list and empty results
    primes((2 to n).toList, List[Int]())
  }

}
