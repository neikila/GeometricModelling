package solver

import constraint.{Axis, Constraint, FixedAxis, Var}
import form.{Form, Point}
import solver.Solver._

import scala.collection.immutable.IndexedSeq
import scala.language.postfixOps

/**
  * Created by k.neyman on 05.01.2017.
  */
class MatrixBuilder(val constraints: List[FixedAxis], val forms: List[Point]) {
  private val xParamAmount: Int = forms.size * 2
  val size = constraints.size + xParamAmount

  val pointsParams = forms.flatMap({ case Point(id, x, y) => (2 * id -> x) :: (2 * id + 1 -> y) :: Nil }).toMap

  val L = new LagrangeFunction(forms, constraints)

  implicit def source(variable: Var): Double = {
    variable match {
      case Var(id, Var.X) => pointsParams(id)
      case Var(id, Var.L) => 1
    }
  }

  def build = {
    val ALeft = for (i <- 0 until xParamAmount; j <- 0 until xParamAmount)
      yield (Var(i, Var.X), Var(j, Var.X))

    val top = ALeft.sliding(xParamAmount, xParamAmount).zipWithIndex.map { case (line, id) =>
      val first = Var(id, Var.X)
      val right: IndexedSeq[(Var, Var)] = constraints.indices.map { i => (first, Var(i, Var.L)) }
      line.toList ::: right.toList
    }.toList
    top ::: constraints.indices.map { id =>
      val first = Var(id, Var.L)
      val left = (0 until xParamAmount).map { i => (first, Var(i, Var.X)) }.toList
      val right = constraints.indices.map { i => (first, Var(i, Var.L)) }.toList
      left ::: right
    }.toList
  }

  def buildB = (0 until xParamAmount).map { Var(_, Var.X) } ++ constraints.indices.map { Var(_, Var.L) }

  def createAMatrix: Matrix = build map { _ map { case (diffBy1, diffBy2) => L.diffBy(diffBy1, diffBy2)} toIndexedSeq } toIndexedSeq

  def createBMatrix: Vector = buildB map { -1 * L.diffBy(_) }
}
