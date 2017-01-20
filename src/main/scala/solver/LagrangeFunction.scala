package solver

import constraint.{Constraint, Differentiable, Var}
import form.Point
import form.Point.PointId
import solver.Solver.Source

/**
  * Created by k.neyman on 12.01.2017.
  */
class LagrangeFunction(points: List[Point], val constraints: List[Constraint], val activePoint: Option[PointId]) extends Differentiable {

  override def f(implicit source: Source): Double = parts.toStream.map(_.f).sum

  def pointToParts(point: Point) = {
    val Point(id, x, y) = point
    val isActive = activePoint.getOrElse(-1) == id
    new SquareDiff(2 * id, x, isActive) :: new SquareDiff(2 * id + 1, y, isActive) :: Nil
  }

  val parts: List[Differentiable] = points.flatMap(pointToParts) ::: constraints

  override def diffBy(diffBy: Var)(implicit source: (Var) => Double): Double =
    parts.toStream.map(_.diffBy(diffBy)).sum

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: (Var) => Double): Double =
    parts.toStream.map(_.diffBy(diffBy1, diffBy2)).sum
}

class SquareDiff(val paramId: Int, val b: Double, isActive: Boolean) extends Differentiable {

  val a = if (isActive) 5000 else 1

  override def f(implicit source: Source): Double = a * (source(Var(paramId, Var.X)) ^ 2 - b)

  override def diffBy(diffBy: Var)(implicit source: (Var) => Double): Double = diffBy match {
    case Var(id, Var.X) if id == paramId => 2 * a * (source(diffBy) - b)
    case _ => 0
  }

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: (Var) => Double): Double = {
    if (diffBy1 == diffBy2) diffBy1 match {
      case Var(id, Var.X) if id == paramId => 2 * a
      case _ => 0
    }
    else 0
  }
}
