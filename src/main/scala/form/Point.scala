package form

import com.sun.org.apache.xpath.internal.operations.Variable
import constraint.{Axis, Support, Var}
import constraint.Axis.Axis
import form.Point.PointId
import solver.Model
import solver.Solver.{ParamId, Source}

/**
  * Created by k.neyman on 05.01.2017.
  */
trait Form

case class Line(from: PointId, to: PointId) extends Form with Support {
  def this(point1: Point, point2: Point) = this(point1.id, point2.id)

  def isFrom(id: ParamId) = Point.isPointCoord(id)(from)
  def isTo(id: ParamId) = Point.isPointCoord(id)(to)
  def isPartOfLine(xParamId: ParamId) = Point.isPointCoord(xParamId)(from) || Point.isPointCoord(xParamId)(to)

  def x(implicit source: Source): Double =
    Point.variable(Axis.X)(to) - Point.variable(Axis.X)(from)

  def y(implicit source: Source) =
    Point.variable(Axis.Y)(to) - Point.variable(Axis.Y)(from)

  def squareLength(implicit source: Source) = (x ^ 2) + (y ^ 2)

  def fromP(implicit points: IndexedSeq[Point]): Point = points(from)
  def toP(implicit points: IndexedSeq[Point]): Point = points(to)
}

case class Point(id: PointId, x: Double, y: Double) extends Form {
  def this(x: Double, y: Double) = this(-1, x, y)

  def += (withP: Point) = Point(id, x + withP.x, y + withP.y)
  def -= (withP: Point) = Point(id, x - withP.x, y - withP.y)
}

object Point {
  type PointId = Int

  def isPointCoord(paramId: ParamId)(implicit pointId: PointId) = {
    val diff = paramId - 2 * pointId
    diff == 0 || diff == 1
  }

  def isPointCoord(paramId: ParamId, axis: Axis.Axis)(implicit pointId: PointId) = axis match {
    case Axis.X => isPointX(paramId)
    case Axis.Y => isPointY(paramId)
  }

  def isX(paramId: ParamId) = paramId % 2 == 0
  def isY(paramId: ParamId) = paramId % 2 == 1
  def isPointX(paramId: ParamId)(implicit pointId: PointId) = paramId - 2 * pointId == 0
  def isPointY(paramId: ParamId)(implicit pointId: PointId) = paramId - 2 * pointId == 1

  def paramIdOf(axis: Axis)(implicit pointId: PointId) =
    if (axis == Axis.X) 2 * pointId
    else 2 * pointId + 1

  def variable(axis: Axis)(implicit pointId: PointId) = Var(paramIdOf(axis), Var.X)
  implicit def variableValue(variable: Var)(implicit source: Source) = source(variable)
}