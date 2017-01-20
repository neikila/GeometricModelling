package constraint

import form.Line
import solver.Solver.{ConstraintId, Source}

/**
  * Created by k.neyman on 20.01.2017.
  */
case class Parallel(override val consId: ConstraintId, l1: Line, l2: Line) extends Constraint {

  override def f(implicit source: Source): Double = l * (l1.x * l2.y - l1.y * l2.x)
}
