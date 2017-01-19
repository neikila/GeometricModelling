package solver

import constraint.Axis.Axis
import constraint.{Constraint, ConstraintConstructor, FixedAxis, FixedLineLength}
import form.Point.PointId
import form.{Form, Line, Point, PointConstructor}
import solver.Solver.Source

/**
  * Created by k.neyman on 18.01.2017.
  */
class Model extends ConstraintConstructor with PointConstructor {

  var points: List[Point] = Nil
  var lines: List[Line] = Nil
  var constraints: List[Constraint] = Nil
  var lastModified: Option[Point] = None
  var recalculated: List[Point] = Nil

  def add(point: Point): Point = {
    points = (point :: points) sortBy(_.id)
    point
  }

  def add(line: Line): Line = {
    lines = line :: lines
    line
  }

  def addLine(p1: Point, p2: Point) = {
    add(new Line(p1, p2))
  }

  def add(constraint: Constraint): Constraint = {
    constraints = (constraint :: constraints) sortBy(_.consId)
    constraint
  }

  def recalculate: Model = {
    import Solver.pointsToIndices
    import Solver.Vector

    implicit val source: Source = Solver.createSource(points, constraints.indices.map(id => 0.0))
    val result: Vector = new NewtonSolver(points, constraints).solve
    recalculated = new ResultExtractor(result, points.size).extract.toList
    this
  }

  def forms: List[Form] = {
    points ::: lines
  }

  def updatePoint(point: Point): Model = {
    points = points.updated(points.indexWhere(_.id == point.id), point)
    lastModified = Some(point)
    this
  }

  def updateConstraint(constraint: Constraint): Model = {
    constraints = constraints.updated(
      constraints.indexWhere(_.consId == constraint.consId),
      constraint
    )
    this
  }

  override def fixedLineLength(line: Line, value: Double): FixedLineLength = {
    val c = super.fixedLineLength(line, value)
    add(c)
    c
  }

  override def fixedAxis(pointId: PointId, axis: Axis, value: Double): FixedAxis = {
    val c = super.fixedAxis(pointId, axis, value)
    add(c)
    c
  }

  override def newPoint(x: Double, y: Double): Point = add(super.newPoint(x, y))
}
