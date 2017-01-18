package form

import form.Point.PointId

/**
  * Created by k.neyman on 18.01.2017.
  */
class PointConstructor {
  var counter: PointId = -1

  def newPoint(x: Double, y: Double): Point = {
    counter += 1
    Point(counter, x, y)
  }
}
