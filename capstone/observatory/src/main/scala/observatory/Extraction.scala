package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.Partitioner
import org.apache.spark.sql.SparkSession

/**
  * 1st milestone: data extraction
  */
object Extraction {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  import org.apache.log4j.{Level, Logger}

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString

  def parseDouble(s: String): Double = try {
    s.toDouble
  } catch {
    case _: Throwable => 0.0d
  }

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationsRdd = spark.sparkContext.textFile(fsPath(stationsFile))
    val temperaturesRdd = spark.sparkContext.textFile(fsPath(temperaturesFile))

    val stationsData = stationsRdd
      .map(_.split(",", -1).to[Array])
      .map(row => {
        (
          Station(row(0), row(1)),
          Location(parseDouble(row(2)), parseDouble(row(3)))
        )
      })
      .filter { row => (row._2.lat != 0.0d) && (row._2.lon != 0.0d) }


    val temperaturesData = temperaturesRdd
      .map(_.split(",", -1).to[Array])
      .map(row => {
        (
          Station(row(0), row(1)),
          (
            LocalDate.of(year, row(2).toInt, row(3).toInt),
            row(4).toDouble
          )
        )
      })

    val partitioner = Partitioner.defaultPartitioner(stationsData, temperaturesData)
    stationsData.join(temperaturesData, partitioner)
      .map { case (station, (location, (localDate, double))) => (localDate, location, double) }
      .toLocalIterator
      .toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records
      .groupBy(_._2)
      .mapValues(iter => iter.foldLeft(0.0)(_ + _._3) / iter.size)
      .mapValues(temp => (temp - 32) / 1.8) //to °C
  }

}
