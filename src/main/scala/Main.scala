import constraint.{Axis, Constraint, FixedAxis, Var}
import form.Point
import solver.Solver.{Matrix, Vector}
import solver._

import scala.language.{implicitConversions, postfixOps}

/**
  * Created by k.neyman on 04.01.2017.
  */

object Main {
  def main(args: Array[String]): Unit = {
    val point = Point(0, 5, 3)
    val c1 = FixedAxis(0, Axis.X, 8, 0)
    val c2 = FixedAxis(1, Axis.Y, 15, 0)

    val constraints: List[FixedAxis] = c1 :: c2 :: Nil
    val forms: List[Point] = point :: Nil

    solve(forms, constraints)
  }

  def solve(points: List[Point], constraints: List[Constraint]) = {
    val pointsParams = points.sortBy(_.id).flatMap({ case Point(id, x, y) => x :: y :: Nil }).toIndexedSeq

    implicit val source: Var => Double =
      Solver.createSource(pointsParams, constraints.indices.map(id => 1.0))

    new ResultExtractor(new NewtonSolver(points, constraints).solve, points.size).extract.foreach(println)
  }
}
