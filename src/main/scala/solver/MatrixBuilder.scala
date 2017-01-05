package solver

import constraint.{Axis, Constraint, FixedAxis}
import form.{Form, Point}

import scala.language.postfixOps

/**
  * Created by k.neyman on 05.01.2017.
  */
class MatrixBuilder(val constraints: List[FixedAxis], forms: List[Point]) {
  private val constraintsWithIndex = constraints zipWithIndex

  val size = constraints.size + forms.size * 2
  val conIndexZero = forms.size * 2

  def build() = {
    val abPoints = forms.toStream.zipWithIndex.map {
      case (point, index) => applyPoint(point, index)
    } reduce(_ ++ _)

    val abConstraints = constraints map applyConstraint reduce(_ ++ _)

    abPoints ++ abConstraints
  }

  def applyConstraint(constraint: FixedAxis) = {
    val column =
      if (constraint.axis == Axis.X) indexOf(constraint.point)
      else indexOf(constraint.point) + 1
    AB(IndexedSeq(IndexedSeq.tabulate(size)(_ => 0.0).updated(column, 1.0)), IndexedSeq(constraint.value))
  }

  private def indexOf(point: Point) = forms.indexOf(point)

  def applyPoint(point: Point, pointIndex: Int) = {
    val cons = constraintsWithIndex filter { case (constraint, index) => constraint.point == point }
    require(cons.size <= 2)

    val xCon = cons.find { case (constraint, _) => constraint.axis == Axis.X }
    val xVector = xCon match {
      case Some((_, index)) =>
        IndexedSeq.tabulate(size)(_ => 0.0)
          .updated(2 * pointIndex, 2.0)
          .updated(conIndexZero + index, -1.0)
      case _ =>
        IndexedSeq.tabulate(size)(_ => 0.0)
          .updated(2 * pointIndex, 2.0)
    }

    val yCon = cons.find { case (constraint, _) => constraint.axis == Axis.Y }
    val yVector = yCon match {
      case Some((_, index)) =>
        IndexedSeq.tabulate(size)(_ => 0.0)
          .updated(2 * pointIndex + 1, 2.0)
          .updated(conIndexZero + index, -1.0)
      case _ =>
        IndexedSeq.tabulate(size)(_ => 0.0)
          .updated(2 * pointIndex + 1, 2.0)
    }

    AB(IndexedSeq(xVector, yVector), IndexedSeq(2 * point.x, 2 * point.y))
  }
}
