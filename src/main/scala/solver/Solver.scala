package solver

import constraint.Var
import form.Point

import scala.language.implicitConversions

/**
  * Created by k.neyman on 04.01.2017.
  */
trait Solver

object Solver {
  type Matrix = IndexedSeq[IndexedSeq[Double]]
  type Vector = IndexedSeq[Double]
  type Source = Var => Double
  type ParamId = Int
  type ConstraintId = Int

  def createSource(xParams: IndexedSeq[Double], lParams: IndexedSeq[Double]): Source = {
    (variable: Var) => {
      variable match {
        case Var(id, Var.X) => xParams(id)
        case Var(id, Var.L) => lParams(id)
        case _ => 0.0 // should never be used
      }
    }
  }

  def createSource(xParams: IndexedSeq[Double]): Source = createSource(xParams, IndexedSeq())

  implicit def pointsToIndices(points: List[Point]): IndexedSeq[Double] =
    points.sortBy(_.id).flatMap({ case Point(id, x, y) => x :: y :: Nil }).toIndexedSeq
}
