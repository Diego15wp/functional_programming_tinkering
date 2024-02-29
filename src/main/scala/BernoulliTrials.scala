import scala.::
import java.io.{PrintWriter, File}
import scala.util.Random


object BernoulliTrials {

  type REAL_RESULT = (Double, Double)
  type RESULTS_2D = List[REAL_RESULT]

  def go = {
    val numTrials = 10
    val fewestFlips = 10
    val mostFlips = 2000
    val r = new Random()
    val results: RESULTS_2D = conductAllTrials(r, numTrials, fewestFlips, mostFlips)
    println(s"results = ${results}")
    val fileName = s"streak_length_for_num_flips_${numTrials}_${fewestFlips}_${mostFlips}.csv"
    val pw = new PrintWriter(new File(fileName))
    results.map(e => pw.write(s"${e._1},${e._2}\n"))
    pw.close()
  }

  def conductAllTrials(r: Random, numTrials: Int, fewestFlips: Int, mostFlips: Int): RESULTS_2D = {
    val data = (fewestFlips to mostFlips).foldLeft(List.empty[REAL_RESULT])({case (ls: List[REAL_RESULT], x) => (x.toDouble, averageMaxStreakLength(r, numTrials, x))::ls})
    val ans = data.reverse
    ans
  }

  def averageMaxStreakLength(r: Random, numTrials: Int, flipsPerTrial: Int): Double = {
    val avg = (1 to numTrials).foldLeft((0.0,0.0))({case ((tot: Double, amount: Double), x) => ((tot+ longestStreak(r, flipsPerTrial), amount+1))})
    avg._1/ avg._2
  }

  def longestStreak(r: Random, numFlips: Int): Int = {

    def update(streak: Int,longest: Int): (Int, Int) = {
      //True Evaluates to heads, every True adds to streak
      if(r.nextBoolean) (streak+1, longest)
      else {(0, streak.max(longest))}   //else reset streak and pick max of streak or longest
    }

    val tup = (1 to numFlips).foldLeft((0,0))({case ((streak:Int, longest: Int), x)=> update(streak, longest) })
    tup._2




  }

}
