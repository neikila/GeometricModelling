package solver

import constraint.{Constraint, Differentiable, Var}
import form.Point

/**
  * Created by k.neyman on 12.01.2017.
  */
class LagrangeFunction(points: List[Point], val constraints: List[Constraint]) extends Differentiable {
  def pointToParts(point: Point) = {
    val Point(id, x, y) = point
    new SquareDiff(2 * id, x) :: new SquareDiff(2 * id + 1, y) :: Nil
  }

  val parts: List[Differentiable] = points.flatMap(pointToParts) ::: constraints

  override def diffBy(diffBy: Var)(implicit source: (Var) => Double): Double =
    parts.toStream.map(_.diffBy(diffBy)).sum

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: (Var) => Double): Double =
    parts.toStream.map(_.diffBy(diffBy1, diffBy2)).sum
}

class SquareDiff(val paramId: Int, val b: Double) extends Differentiable {
  override def diffBy(diffBy: Var)(implicit source: (Var) => Double): Double = diffBy match {
    case Var(id, Var.X) if id == paramId => 2 * (source(diffBy) - b)
    case _ => 0
  }

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: (Var) => Double): Double = {
    if (diffBy1 == diffBy2) diffBy1 match {
      case Var(id, Var.X) if id == paramId => 2
      case _ => 0
    }
    else 0
  }
}
