package constraint

import constraint.Axis.Axis
import form.{Line, Point}
import form.Point.PointId
import solver.Solver.ConstraintId

/**
  * Created by k.neyman on 18.01.2017.
  */
class ConstraintConstructor {
  var counter: ConstraintId = -1

  def fixedAxis(point: Point, axis: Axis, value: Double): FixedAxis =
    fixedAxis(point.id, axis, value)

  def fixedAxis(pointId: PointId, axis: Axis, value: Double): FixedAxis = {
    counter += 1
    FixedAxis(counter, axis, value, pointId)
  }

  def fixedLineLength(line: Line, value: Double): FixedLineLength = {
    counter += 1
    FixedLineLength(counter, line, value)
  }
}
