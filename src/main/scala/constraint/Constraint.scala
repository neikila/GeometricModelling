package constraint

import constraint.Axis.Axis
import constraint.Var.VarType
import form.{Line, Point}
import form.Point.PointId
import solver.Solver.{ConstraintId, ParamId, Source}





trait Constraint extends Differentiable {
  val consId: ConstraintId
}







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

case class FixedLineLength(consId: ConstraintId, line: Line, length: Double) extends Constraint {

  override def f(implicit source: Source): Double = ((length ^ 2) - (line.x ^ 2) - (line.y ^ 2)) * l

  def depend(id: ParamId): Boolean = line.isPartOfLine(id)
  def depend(variable: Var): Boolean = variable match {
    case Var(id, Var.X) => depend(id)
    case Var(id, Var.L) => id == consId
    case _ => false
  }

  override def diffBy(diffBy: Var)(implicit source: Source): Double = diffBy match {
    case _ if !depend(diffBy) => 0
    case Var(id, Var.L) => (length ^ 2) - (line.x ^ 2) - (line.y ^ 2)
    case Var(id, Var.X) => l * dLdldx(id)
  }

  private def dLdldx(id: ParamId)(implicit source: Source) = {
    val sign = if (line.isFrom(id)) -1 else 1
    sign * 2 * { if (id % 2 == 0) -1 * line.x else -1 * line.y }
  }

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: Source): Double = {
    (diffBy1, diffBy2) match {
      case (any1, any2) if !depend(any1) || !depend(any2) => 0
      case (Var(id1, Var.L), Var(id2, Var.X)) => dLdldx(id2)
      case (Var(id1, Var.X), Var(id2, Var.L)) => dLdldx(id1)
      case (Var(_, Var.L), Var(_, Var.L)) => 0
      case (Var(id1, Var.X), Var(id2, Var.X)) if id1 == id2 => -2 * l
      case (Var(id1, Var.X), Var(id2, Var.X)) =>
        if (line.isFrom(id1) == line.isTo(id2) && Point.isX(id1) == Point.isX(id2)) 2 * l
        else 0
      case _ => 0
    }
  }

  def l(implicit source: Source) = source(Var(consId, Var.L))

  def squareLength = length ^ 2
}