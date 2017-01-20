package constraint

import solver.Solver.{ConstraintId, Source}

trait Constraint extends DiffApproximation {
  val consId: ConstraintId

  def l(implicit source: Source) = source(Var(consId, Var.L))
}