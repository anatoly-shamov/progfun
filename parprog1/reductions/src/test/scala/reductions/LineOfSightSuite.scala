package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("downsweep should correctly handle trees with a single leaf") {
    val output = new Array[Float](5)
    downsweep(Array[Float](0.0f, 7.0f, 10.0f, 33.0f, 40.0f), output, 0f, Leaf(0, 5, 0.0f))
    assert(output.toList == List(0.0f, 7.0f, 7.0f, 11.0f, 11.0f))
  }

  test("downsweep should correctly compute the output for a tree with 4 leaves when the starting angle is zero") {
    val output = new Array[Float](5)
    downsweep(Array[Float](0.0f, 7.0f, 10.0f, 33.0f, 48.0f), output, 0f, Node(Node(Leaf(1, 2, 7.0f), Leaf(2, 3, 5.0f)), Node(Leaf(3, 4, 11.0f), Leaf(4, 5, 12.0f))))
    assert(output.toList == List(0.0f, 7.0f, 7.0f, 11.0f, 12.0f))
  }
}

