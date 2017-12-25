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
    val length = 10000000
    val chars = new Array[Char](length)
    val threshold = 1000
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
    def loop(counter: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) counter == 0
      else if (chars.head == '(') loop(counter + 1, chars.tail)
      else if (chars.head == ')') {
        if(counter == 0) false
        else loop(counter - 1, chars.tail)
      }
      else loop(counter, chars.tail)
    }
    loop(0, chars.toList)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) : (Int, Int) = {
      if (idx == until) (arg1, arg2)
      else if (chars(idx) == '(') traverse(idx + 1, until, arg1 + 1, arg2)
      else if (chars(idx) == ')') traverse(idx + 1, until, arg1 - 1, if (arg1 == arg2) arg2 - 1 else arg2)
      else traverse(idx + 1, until, arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
    if (until - from <= threshold)
      traverse(from, until, 0, 0)
    else {
      val mid = from + (until - from) / 2
      val ((arg1_1, arg2_1), (arg1_2, arg2_2)) =
        parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
      (arg1_1 + arg1_2, math.min(arg2_1, arg1_1 + arg2_2))
    }
  }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
