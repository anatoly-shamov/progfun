package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite {

  test("load 2015 temperature data"){
    val data = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
    println(s"loaded ${data.size} lines of data")
    val averaged = Extraction.locationYearlyAverageRecords(data)
    println(s"averaged ${averaged.size} lines of data")
    //010010,,+70.933,-008.667
    val res = averaged.find{case (location, double) => location.equals(Location(70.933, -8.667))}.get
    println(s"avg for ${res._1} = ${res._2}")
  }
}