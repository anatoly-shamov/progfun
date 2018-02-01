package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.spark.sql.SparkSession

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  def distanceInKm(loc1: Location, loc2: Location): Double = {
    val EARTH_RADIUS = 6371
    val a = Math.pow(Math.sin(Math.toRadians(loc1.lat - loc2.lat) / 2), 2) +
      Math.pow(Math.sin(Math.toRadians(loc1.lon - loc2.lon) / 2), 2) *
        Math.cos(Math.toRadians(loc1.lat)) * Math.cos(Math.toRadians(loc2.lat))
    val c = 2 * Math.asin(Math.sqrt(a))
    EARTH_RADIUS * c
  }

  val P_POWER = 8
  def weight(distance: Double): Double = 1 / Math.pow(distance, P_POWER)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    println(s"processing (${location.lat}, ${location.lon})")

    def exactLocation(dist: Double): Boolean = dist < 1

    val weighted = temperatures.par
      .map { case (loc, temp) => (loc, temp, distanceInKm(loc, location)) }
      .map { case (loc, temp, dist) =>
      (loc, temp, dist, if (exactLocation(dist)) 0.0 else weight(dist))
    }

    val exactLocations = weighted.filter(t => exactLocation(t._3))
    if (exactLocations.nonEmpty)
      exactLocations.head._2
    else {
      val denominator = weighted.aggregate(0.0)({ case (acc2, t2) => acc2 + t2._4 }, _ + _)
      weighted.aggregate(0.0)({ case (acc1, t1) => acc1 + t1._2 * t1._4 / denominator }, _ + _)
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val ceil = points.filter(_._1 > value) match {
      case iter@head +: tail => iter.minBy(_._1)
      case _ => (value, Color(255, 255, 255))
    }
    val floor = points.filter(_._1 < value) match {
      case iter@head +: tail => iter.maxBy(_._1)
      case _ => (value, Color(0, 0, 0))
    }

    val k = (value - floor._1) / (ceil._1 - floor._1)
    Color(
      (Math.min(ceil._2.red, floor._2.red) + Math.abs(ceil._2.red - floor._2.red) * k).toInt,
      (Math.min(ceil._2.green, floor._2.green) + Math.abs(ceil._2.green - floor._2.green) * k).toInt,
      (Math.min(ceil._2.blue, floor._2.blue) + Math.abs(ceil._2.blue - floor._2.blue) * k).toInt
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val interpolated = for {
      lat <- Range(90, -90, -1)
      lon <- Range(-180, 180)
    } yield interpolateColor(
      colors,
      predictTemperature(temperatures, Location(lat, lon))
    )

    val pixels = interpolated.map(color => Pixel(color.red, color.green, color.blue, 255)).toArray

    Image(360, 180, pixels)
  }

}

