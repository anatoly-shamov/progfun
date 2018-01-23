package stackoverflow

import org.apache.spark.{SparkConf, SparkContext}
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("scored RDD should have 2121822 entries") {
    @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
    @transient lazy val sc: SparkContext = new SparkContext(conf)

    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw     = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    val scored  = testObject.scoredPostings(grouped)
    val vectors = testObject.vectorPostings(scored)

    assert(vectors.count() == 2121822, "Incorrect number of vectors: " + vectors.count())
  }


}
