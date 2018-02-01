package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Visualization.{interpolateColor, predictTemperature}
import org.apache.spark.sql.SparkSession

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()
  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = tile.toLocation

  def zoomToSubtiles(tile: Tile, zoom: Int): Iterable[Tile] = {
    val subtile1 = Tile(tile.x * 2, tile.y * 2, tile.zoom + 1)
    val subtile2 = Tile(tile.x * 2 + 1, tile.y * 2, tile.zoom + 1)
    val subtile3 = Tile(tile.x * 2, tile.y * 2 + 1, tile.zoom + 1)
    val subtile4 = Tile(tile.x * 2 + 1, tile.y * 2 + 1, tile.zoom + 1)
    if (zoom == 0) {
      Array(subtile1, subtile2, subtile3, subtile4)
    } else {
      zoomToSubtiles(subtile1, zoom - 1) ++
        zoomToSubtiles(subtile2, zoom - 1) ++
        zoomToSubtiles(subtile3, zoom - 1) ++
        zoomToSubtiles(subtile4, zoom - 1)
    }
  }

  def tileSortFunction(zoom: Int): Tile => Int = t => t.x + t.y * math.pow(2, zoom).toInt

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val TILES_DEPTH = 8 //2^8 = 256

    val pixels = spark.sparkContext
      .parallelize(zoomToSubtiles(tile, TILES_DEPTH - 1).toStream)
      .sortBy(tileSortFunction(TILES_DEPTH))
      .map(t => interpolateColor(
          colors,
          predictTemperature(temperatures, t.toLocation)
        ))
      .map(color => Pixel(color.red, color.green, color.blue, 127))
      .collect()

    Image(256, 256, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    val TILES_DEPTH = 3
    val tilesRdd = spark.sparkContext
      .parallelize(zoomToSubtiles(Tile(0, 0, 0), TILES_DEPTH - 1).toStream)
      .sortBy(tileSortFunction(TILES_DEPTH))
    val dataRdd = spark.sparkContext
      .parallelize(yearlyData.toSeq)

    dataRdd.repartition(4).cache().take(1)
    dataRdd.cartesian(tilesRdd).foreach{ case ((year, data), tile) => generateImage(year, tile, data)}
  }
}
