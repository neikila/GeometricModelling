package constraint

import constraint.Axis.Axis
import form.Point
import form.Point.PointId
import solver.Solver.{ConstraintId, ParamId, Source}

/**
  * Created by k.neyman on 20.01.2017.
  */
case class FixedAxis(consId: ConstraintId, axis: Axis, value: Double, implicit val pointId: PointId) extends Constraint {

  override def f(implicit source: Source): Double = {
    source(Var(consId, Var.L)) * (value - source(Var(pointId, Var.X)))
  }

  override def diffBy(diffBy: Var)(implicit source: Source): Double = {
    val Var(index, varType) = diffBy
    varType match {
      case Var.L =>
        if (index == consId) value - source(Point.variable(axis))
        else 0
      case Var.X =>
        if (Point.isPointCoord(index, axis)) -source(Var(consId, Var.L))
        else 0
    }
  }

  def diffBy(lambdaId: ConstraintId, xId: ParamId) =
    if (lambdaId == consId && Point.isPointCoord(xId, axis)) -1
    else 0

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: Source): Double = {

    (diffBy1, diffBy2) match {
      case (Var(lambdaId, Var.L), Var(xId, Var.X)) => diffBy(lambdaId, xId)
      case (Var(xId, Var.X), Var(lambdaId, Var.L)) => diffBy(lambdaId, xId)
      case _ => 0
    }
  }
}
