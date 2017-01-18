package constraint

import constraint.Var.VarType
import solver.Solver.ParamId

/**
  * Created by k.neyman on 18.01.2017.
  */
case class Var(paramIndex: ParamId, varType: VarType) {
  override def toString: String = s"$varType${paramIndex + 1}"
}

object Var extends Enumeration {
  type VarType = Value
  val X, L = Value
}