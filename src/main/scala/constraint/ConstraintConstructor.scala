package constraint

import constraint.Axis.Axis
import form.{Line, Point}
import form.Point.PointId
import solver.Solver.ConstraintId

/**
  * Created by k.neyman on 18.01.2017.
  */
trait ConstraintConstructor {
  var consCounter: ConstraintId = -1

  def fixedAxis(point: Point, axis: Axis, value: Double): FixedAxis =
    fixedAxis(point.id, axis, value)

  def fixedAxis(pointId: PointId, axis: Axis, value: Double): FixedAxis = {
    consCounter += 1
    FixedAxis(consCounter, axis, value, pointId)
  }

  def fixedLineLength(line: Line, value: Double): FixedLineLength = {
    consCounter += 1
    FixedLineLength(consCounter, line, value)
  }
}
