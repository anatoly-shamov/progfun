package observatory

import org.apache.log4j.Logger

import scala.util.Random

object Main extends App {

  val mappedColors = Array(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  println("work started")
  val data = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
  println(s"data extracted, ${data.size} entries")
  val averaged = Extraction.locationYearlyAverageRecords(data)
  val averagedSample = Random.shuffle(averaged.take(averaged.size / 4))
  println(s"data averaged, ${averaged.size} entries, took ${averagedSample.size}")
  val image = Visualization.visualize(averagedSample, mappedColors)
  println("data visualized")
  image.output(new java.io.File("target/2015.png"))

}
