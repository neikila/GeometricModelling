package solver

import constraint.Axis.Axis
import constraint._
import form.Point.PointId
import form.{Form, Line, Point, PointConstructor}
import Solver.{Source, Vector, pointsToIndices}


/**
  * Created by k.neyman on 18.01.2017.
  */
case class Model(var points: List[Point],
                 var lines: List[Line],
                 var constraints: List[Constraint]) extends ConstraintConstructor with PointConstructor {

  def this() = this(Nil, Nil, Nil)

  val pointsAr = points.toIndexedSeq
  val source = Solver.createSource(points, Nil)

  var lastModified: Option[Point] = None

  def add(point: Point): Point = {
    points = (point :: points) sortBy (_.id)
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
    constraints = (constraint :: constraints) sortBy (_.consId)
    constraint
  }

  def recalculate: Model = {
    implicit val source: Source = Solver.createSource(points, constraints.indices.map(id => 0.0))
    val result: Vector = new NewtonSolver(points, constraints).solve
    val recalculated = new ResultExtractor(result, points.size).extract.toList
    copy(points = recalculated)
  }

  def forms: List[Form] = points ::: lines

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

  override def parallel(l1: Line, l2: Line): Parallel = {
    val c = super.parallel(l1, l2)
    add(c)
    c
  }

  override def orto(l1: Line, l2: Line): Orto = {
    val c = super.orto(l1, l2)
    add(c)
    c
  }

  override def fixedAngle(l1: Line, l2: Line, angle: Double): FixedAngle = {
    val c = super.fixedAngle(l1, l2, angle)
    add(c)
    c
  }

  override def newPoint(x: Double, y: Double): Point = add(super.newPoint(x, y))
}
