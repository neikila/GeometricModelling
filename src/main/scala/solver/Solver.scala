package solver

import constraint.Var

/**
  * Created by k.neyman on 04.01.2017.
  */
trait Solver

object Solver {
  type Matrix = IndexedSeq[IndexedSeq[Double]]
  type Vector = IndexedSeq[Double]

  def createSource(xParams: IndexedSeq[Double], lParams: IndexedSeq[Double]): Var => Double = {
    (variable: Var) => {
      variable match {
        case Var(id, Var.X) => xParams(id)
        case Var(id, Var.L) => lParams(id)
      }
    }
  }
}
