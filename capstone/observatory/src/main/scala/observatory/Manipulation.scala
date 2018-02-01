package observatory

import org.apache.spark.sql.SparkSession
import org.apache.spark.storage.StorageLevel

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  def memoize[A, B](f: A => B): (A => B) = {
    val cache = collection.concurrent.TrieMap[A, B]()
    (a: A) => cache.getOrElseUpdate(a, f(a))
  }
  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    memoize(gridLocation =>
      Visualization.predictTemperature(temperatures, gridLocation.toLocation)
    )
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val temperaturessRdd = spark.sparkContext
      .parallelize(temperaturess.toStream)
      .map(temperatures => makeGrid(temperatures))
      .persist(StorageLevel.MEMORY_ONLY_SER)

    gridLocation => {
      val gridded = temperaturessRdd.map(f => f(gridLocation)).persist(StorageLevel.MEMORY_ONLY_SER)
      gridded.fold(0.0)((t1, t2) => t1 + t2) / gridded.count()
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    gridLocation => makeGrid(temperatures)(gridLocation) - normals(gridLocation)
  }
}
