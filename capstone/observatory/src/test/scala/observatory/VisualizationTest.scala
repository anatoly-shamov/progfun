package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  test("distance calculation - regular case") {
    val loc1 = Location(15.25, -110.01)
    val loc2 = Location(-60.81, 80.33)
    val sampleDist = 14880
    val error = 0.001
    assert(Math.abs(Visualization.distanceInKm(loc1, loc2) - sampleDist) < sampleDist * error)
  }

  test("distance calculation - same locations") {
    val loc1 = Location(15.25, -110.01)
    val sampleDist = 14880
    val error = 0.001
    assert(Visualization.distanceInKm(loc1, loc1) == 0.0)
  }

  test("distance calculation - antipodes") {
    val loc1 = Location(15.25, -110.01)
    val loc2 = Location(-15.25, 180 - 110.01)
    val sampleDist = Math.PI * 6371
    val error = 0.001
    assert(Math.abs(Visualization.distanceInKm(loc1, loc2) - sampleDist) < sampleDist * error)
  }

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

  test("colors interpolation - regular case") {
    val testTemp = 6.0
    assert(Visualization.interpolateColor(mappedColors, testTemp) == Color(127, 255, 127))
  }

  test("colors interpolation - corner case max") {
    val testTemp = 66.0
    assert(Visualization.interpolateColor(mappedColors, testTemp) == Color(255, 255, 255))
  }

  test("colors interpolation - corner case min") {
    val testTemp = -66.0
    assert(Visualization.interpolateColor(mappedColors, testTemp) == Color(0, 0, 0))
  }
}
