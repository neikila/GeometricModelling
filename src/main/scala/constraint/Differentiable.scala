package constraint

import solver.Solver.Source

/**
  * Created by k.neyman on 05.01.2017.
  */
trait Differentiable extends Support {
  def f(implicit source: Source): Double

  def diffBy(diffBy: Var)(implicit source: Source): Double

  def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: Source): Double
}
