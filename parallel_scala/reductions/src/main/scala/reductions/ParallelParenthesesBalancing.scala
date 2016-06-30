package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    var pars = 0
    for(x <- chars)
    {
      if(x == '(') {
        pars += 1
      }
      if(x == ')') {
        pars -= 1
      }
      if(pars <0){
        return  false
      }
    }
    pars == 0
  }

  def countPars(chars: Array[Char], idx: Int, until: Int): Int =  {
    var pars = 0
    for (x<- idx to until){
      if(x == '(') {
        pars += 1
      }
      if(x == ')') {
        pars -= 1
      }
    }
    pars
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      if(chars.length > threshold)
        {

          val middle = (until - idx) /2
          val (a, b) = parallel( countPars(chars, idx, until -middle), countPars(chars, until- middle, until))
          a+b
        }else
        {
          countPars(chars, idx, until)
        }
    }

    def reduce(from: Int, until: Int) /*: ???*/ = {
      traverse(from, until, 0, 0)
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
