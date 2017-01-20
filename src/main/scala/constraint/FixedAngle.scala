package constraint

import form.Line
import solver.Solver.{ConstraintId, Source}

/**
  * Created by k.neyman on 20.01.2017.
  */
case class FixedAngle(override val consId: ConstraintId, l1: Line, l2: Line, angle: Double) extends Constraint {
  val angleInRad = angle / 180 * math.Pi

  override def f(implicit source: Source): Double =
    l * (l1.length * l2.length * math.cos(angleInRad) - (l1.x * l2.x + l1.y * l2.y))
}
