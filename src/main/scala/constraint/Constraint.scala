package constraint

import constraint.Axis.Axis
import form.Point

/**
  * Created by k.neyman on 05.01.2017.
  */
trait Constraint

object Axis extends Enumeration {
  type Axis = Value
  val X, Y = Value
}

case class FixedAxis (axis: Axis, value: Double, point: Point) extends Constraint