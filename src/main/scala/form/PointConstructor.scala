package form

import form.Point.PointId

/**
  * Created by k.neyman on 18.01.2017.
  */
trait PointConstructor {
  var pointerCounter: PointId = -1

  def newPoint(x: Double, y: Double): Point = {
    pointerCounter += 1
    Point(pointerCounter, x, y)
  }
}
