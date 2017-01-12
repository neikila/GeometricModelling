package constraint

import constraint.Axis.Axis
import constraint.Var.VarType
import form.Point
import form.Point.PointId

/**
  * Created by k.neyman on 05.01.2017.
  */
trait Differentiable {
  def diffBy(diffBy: Var)(implicit source: Var => Double): Double
  def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: Var => Double): Double
}

trait Constraint extends Differentiable

object Axis extends Enumeration {
  type Axis = Value
  val X, Y = Value
}

object Var extends Enumeration {
  type VarType = Value
  val X, L = Value
}

case class Var(paramIndex: Int, varType: VarType) {
  override def toString: String = s"$varType$paramIndex"
}
case class FixedAxis(id: Int, axis: Axis, value: Double, implicit val pointId: PointId) extends Constraint {

  override def diffBy(diffBy: Var)(implicit source: (Var) => Double): Double = {
    val Var(index, varType) = diffBy
    varType match {
      case Var.L =>
        if (index == id) value - source(Point.variable(axis))
        else 0
      case Var.X =>
        if (Point.its(index, axis)) -source(Var(id, Var.L))
        else 0
    }
  }

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: (Var) => Double): Double = {
    def diffBy(lambdaId: Int, xId: Int) =
      if (lambdaId == id && Point.its(xId, axis)) -1
      else 0

    (diffBy1, diffBy2) match {
      case (Var(lambdaId, Var.L), Var(xId, Var.X)) => diffBy(lambdaId, xId)
      case (Var(xId, Var.X), Var(lambdaId, Var.L)) => diffBy(lambdaId, xId)
      case _ => 0
    }
  }
}