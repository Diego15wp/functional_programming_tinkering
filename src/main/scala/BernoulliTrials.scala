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

  def conductAllTrials(r: Random, numTrials: Int, fewestFlips: Int, mostFlips: Int): RESULTS_2D = ???

  def averageMaxStreakLength(r: Random, numTrials: Int, flipsPerTrial: Int): Double = ???

  def longestStreak(r: Random, numFlips: Int): Int = ???

}
