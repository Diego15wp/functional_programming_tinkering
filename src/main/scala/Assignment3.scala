import scala.util.Random
import SieveEratosthenes.allPrimes
import ReverseLinkedList.{viewList, reverseListFunctional}
import Multiples3Or5.getMultiples
import BernoulliTrials.{conductAllTrials, RESULTS_2D}

object Assignment3 {

  // Task 1. lambda expression for PI approximation
  // i.
  val pi_approximation :Seq[(Double,Double)] => Double = (sq: Seq[(Double, Double)]) => ((4 * sq.count(x=> Math.sqrt(x._1*x._1 + x._2*x._2) < 1)).toDouble / sq.size)
    //val siz = sq.size
    //val second = sq.filter(x=> Math.sqrt(x._1*x._1 + x._2*x._2) < 1)
    //(4 * sq.filter(x=> Math.sqrt(x._1*x._1 + x._2*x._2) < 1).size) / sq.size
  // ii.
  val values: Random => Int => Seq[(Double,Double)] = (r:Random) => (num:Int) => (1 to num).toList.map(x => (r.nextDouble(),r.nextDouble()))


  // 2. approximate PI using a stream of values, wrapped in an option
  // i.
  val streamed_values: Random => Stream[(Double,Double)] = (r:Random) => {def f(rand: Random):Stream[(Double,Double)] = (rand.nextDouble(),rand.nextDouble)#::f(rand);f(r)}

  // ii.
  def pi_approximation_option(v: Seq[(Double,Double)])(n: Int): Option[Double] = {
    try Some(4* v.take(n).toList.count(x=> Math.sqrt(x._1*x._1 + x._2*x._2) < 1).toDouble / n)
    catch{case e:Exception => None}

  }

  // 3. Lifting assignment 2 items
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // i.
  // Sieve of Eratosthenes
  val allPrimesOption: Option[Int] => Option[List[Int]] = lift(allPrimes)

  // Reverse Linked List
  val reverseListOption: Option[ListNode] => Option[ListNode] = lift(reverseListFunctional)

  // Multiples of 3 or 5
  val getMultiplesOption: Option[Int] => Option[Int] = lift(getMultiples)

  // ii.
  // Bernoulli trials
  val conductAllTrialsOption: (Option[Random],Option[Int],Option[Int],Option[Int]) => Option[RESULTS_2D] =(a:Option[Random],b:Option[Int], c:Option[Int], d:Option[Int]) => (a,b,c,d) match {
    case (( Some(w),Some(x),Some(y),Some(z))) => Some(conductAllTrials(w,x,y,z))
    case _ => None;}

  // 4. Corecursion applied to recurrence relations
  // i. factorials
  val factorials: Stream[Int] = {
    def f(i:Int, h:Int):Stream[Int] = {
      i#::f((h+1)*i, (h+1));
    }
    f(1,0);
  }

  // ii. Fibonacci numbers: t_1 = 1, t_2 = 1, t_n = t_n-1 + t_n-2
  val fibs: Stream[Int] = {
    def f(f0: Int, f1: Int): Stream[Int] = {
      f0#::f(f1,f0+f1)
    }
    f(1,1)
  }

  // iii. a third order recurrence relation: t_1 = 2, t_2 = 3, t_3 = 5, t_n = 2 * t_n-1 + 7 * t_n-2 + 9 * t_n-3
  val t: Stream[Int] = {
    def f(f0: Int, f1:Int, f2:Int):Stream[Int] ={
      val rec = (x:Int, y:Int, z:Int)=> (2*x+7*y+9*z)
      val first = rec(f2,f1,f0)
      val second = rec(first, f2, f1)
      val third = rec(second, first, f2)
      f0#::f1#::f2#::f(first, second, third)
    }
    f(2, 3, 5)
  }

  // iv.
  // define using corecursion an infinite stream of positive integers starting with 2, i.e. [2,3,4,5,6,7,...]
  val naturals: Stream[Int] = {
    def f(x:Int):Stream[Int] = {
      x #:: f(x+1)
    }
    f(2)
  }
  // define using corecursion an infinite stream of primes numbers starting with 2, i.e. [2,3,5,7,11,13,17,19,11,13,17,...]
  val primes: Stream[Int] = {
    def  f(a: Stream[Int]):Stream[Int]= {
      a.head #:: f(a.tail.filter(x=> x % a.head  != 0 ))
    }
    f(naturals)
  }

  // 5. Error handling using flatMap chaining with Either
  trait MiscellaneousError
  object ItemNotFoundError extends MiscellaneousError
  object TypeConversionError extends MiscellaneousError
  object UndefinedSlopeError extends MiscellaneousError
  val m: Map[String,Tuple4[String,String,String,String]] = Map(
    "slope one" -> ("0.0","0.0","1.0","1.0"),
    "slope zero" -> ("1.0","1.0","2.0","1.0"),
    "undefined slope" -> ("2.0","2.0","2.0","3.0"),
    "bad values" -> ("foo","bar","baz","biz")
  )
  val findValues: String => Either[MiscellaneousError,Tuple4[String,String,String,String]] = (s:String) => {
    try {Right(m(s))}
    catch{
      case (e:Exception)=> Left(ItemNotFoundError)
    }
  }

  val convertValues: Tuple4[String,String,String,String] => Either[MiscellaneousError,Tuple4[Double,Double,Double,Double]] = (x:Tuple4[String, String, String, String]) =>{
    try{Right((x._1.toDouble,x._2.toDouble,x._3.toDouble,x._4.toDouble))}
    catch{
      case e:Exception => Left(TypeConversionError)
    }
  }

  val slope: Tuple4[Double,Double,Double,Double] => Either[MiscellaneousError,Double] = (x:Tuple4[Double, Double, Double, Double]) =>{
    if(x._1 == x._3) Left(UndefinedSlopeError)
    else Right((x._4- x._2)/ (x._3 - x._1))

  }

  // DO NOT MODIFY THE FUNCTION BELOW
  def go = {
    println("Assignment 3")
    println("Task 1")
    println(s"For 1000 values, pi approximates to ${pi_approximation(values(Random)(1000))}")
    println(s"For 10000 values, pi approximates to ${pi_approximation(values(Random)(10000))}")
    println(s"For 100000 values, pi approximates to ${pi_approximation(values(Random)(100000))}")
    println(s"For 1000000 values, pi approximates to ${pi_approximation(values(Random)(1000000))}")
    println(s"Task 2")
    println(s"For 1000 streamed values, pi approximates to ${pi_approximation_option(streamed_values(Random))(1000).getOrElse(0.0)}")
    println(s"For 10 values but stream is empty, pi approximates to ${pi_approximation_option(Stream())(10).getOrElse(0.0)}")
    println(s"For 10 values but stream is not big enough, pi approximates to ${pi_approximation_option(Stream((0.3,0.4),(0.5,0.9)))(10).getOrElse(0.0)}")
    println(s"For 10 values using a List that isn't big enough, pi approximates to ${pi_approximation_option(List((0.2,0.3),(0.4,0.6)))(10).getOrElse(0.0)}")
    println("Task 3")
    println(s"All primes smaller than 30: ${SieveEratosthenes.allPrimes(30)}")
    println(s"Using options, all primes smaller than 30: ${allPrimesOption(Some(30))}")
    println(s"Using options, all primes smaller than None: ${allPrimesOption(None)}")
    println("Reverse linked list")
    val res1 = ReverseLinkedList.viewList(ReverseLinkedList.reverseListFunctional(
        new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))))
    println(s"Reversing linked list using original function: ${res1}")
    val res2 = reverseListOption(Some(new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, null))))))
      .map(viewList)
    println(s"Reversing linked list using lifted function: ${res2}")
    println("Multiples of 3 or 5")
    println(s"Sum for n = 10, ${Multiples3Or5.getMultiples(10)}")
    println(s"Using lifted function, sum for n = 10: ${getMultiplesOption(Some(10))}")
    println("Bernoulli Trials")
    val res3 = BernoulliTrials.conductAllTrials(Random, 10, 10, 100)
    val res4 = conductAllTrialsOption(Some(Random), Some(10), Some(10), Some(100))
    println(s"Trial results for numTrials = 10, fewestFlips = 10, mostFlips = 100: ${res3}")
    println(s"Using lifted function, trial results for numTrials = 10, fewestFlips = 10, mostFlips = 100: ${res4}")
    println("Task 4")
    println("First ten factorials (manually computed): 1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880")
    println(s"First ten factorials (computed using corecursion): ${factorials.take(10).toList}")
    println("First ten Fibonacci numbers (manually computed): 1, 1, 2, 3, 5, 8, 13, 21, 34, 55")
    println(s"First ten Fibonacci numbers (computed using corecursion): ${fibs.take(10).toList}")
    println("First ten values of recurrence (manually computed): 2, 3, 5, 49, 160, 708, 2977, 12350, 51911, 217065")
    println(s"First ten values of recurrence t (computed using corecursion): ${t.take(10).toList}")
    println("First 10 natural numbers starting with 2: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11")
    println(s"First 10 natural numbers starting with 2 (computed using corecursion): ${naturals.take(10).toList}")
    println("First 20 primes (computed manually): 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71")
    println(s"First 20 primes (computed using corecursion): ${primes.take(20).toList}")
    println("Task 5")
    println(s"Slope one: ${findValues("slope one").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Slope zero: ${findValues("slope zero").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Invalid key: ${findValues("foo").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Bad values: ${findValues("bad values").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
    println(s"Undefined slope: ${findValues("undefined slope").flatMap(convertValues).flatMap(slope).fold(left => left.toString, right => right.toString)}")
  }
}
