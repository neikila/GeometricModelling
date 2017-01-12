package form

import constraint.{Axis, Var}
import constraint.Axis.Axis

/**
  * Created by k.neyman on 05.01.2017.
  */
trait Form

case class Point(id: Int, x: Double, y: Double) extends Form

object Point {
  type PointId = Int

  def isParam(paramId: Int)(implicit pointId: PointId) = {
    val diff = paramId - 2 * pointId
    diff == 0 || diff == 1
  }

  def its(paramId: Int, axis: Axis.Axis)(implicit pointId: PointId) = axis match {
    case Axis.X => isX(paramId)
    case Axis.Y => isY(paramId)
  }
  def isX(paramId: Int)(implicit pointId: PointId) = paramId - 2 * pointId == 0
  def isY(paramId: Int)(implicit pointId: PointId) = paramId - 2 * pointId == 1

  def paramId(axis: Axis)(implicit pointId: PointId) =
    if (axis == Axis.X) 2 * pointId
    else 2 * pointId + 1

  def variable(axis: Axis)(implicit pointId: PointId) = Var(paramId(axis), Var.X)
}