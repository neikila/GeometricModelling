package constraint

import solver.Solver.ConstraintId

trait Constraint extends DiffApproximation {
  val consId: ConstraintId
}