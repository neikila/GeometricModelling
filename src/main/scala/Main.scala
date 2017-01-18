import constraint._
import form.{Line, Point, PointConstructor}
import solver.Solver.{Source, Vector}
import solver._

import scala.language.{implicitConversions, postfixOps}

object Main extends Support {
  val constraintConstructor = new ConstraintConstructor
  val pointConstructor = new PointConstructor

  import Solver.pointsToIndices

  def main(args: Array[String]): Unit = {
    val point = pointConstructor.newPoint(5, 7)
    val point2 = pointConstructor.newPoint(5, 120)

    val c1 = constraintConstructor.fixedAxis(point, Axis.X, 5)
    val c2 = constraintConstructor.fixedAxis(point, Axis.Y, 0)

    val line = new Line(point, point2)
    val c3 = constraintConstructor.fixedLineLength(line, 9)

    val constraints: List[Constraint] = c1 :: c2 :: c3 :: Nil
    val forms: List[Point] = point :: point2 :: Nil
    solve(forms, constraints)
  }

  def solve(points: List[Point], constraints: List[Constraint]) = {
    implicit val source: Source = Solver.createSource(points, constraints.indices.map(id => 0.0))

    val result: Vector = new NewtonSolver(points, constraints).solve

    new ResultExtractor(result, points.size).extract
  }
}
