package solver

import constraint.{Constraint, Var}
import form.Point
import solver.Solver.{Matrix, Source, Vector}

import scala.language.implicitConversions

/**
  * Created by k.neyman on 11.01.2017.
  */
class NewtonSolver(points: List[Point], constraints: List[Constraint]) extends Solver with Accuracy {
  private val xParamsAmount: Int = 2 * points.size

  implicit def idToVar(id: Int): Var = {
    if (id < xParamsAmount) Var(id, Var.X)
    else Var(id - xParamsAmount, Var.L)
  }

  val builder = new MatrixBuilder(constraints, points)

  def solve(implicit source: Source): Vector = {
    val aMatrix = builder.createAMatrix
    val bVector = builder.createBVector

    val delta = new Gaus(aMatrix, bVector).result.B

    if (shouldStop(delta)) {
      println("Achieved!")
      delta.zipWithIndex.map { case (value, id) => value + source(id) }
    }
    else
      solve(createSource(delta))
  }

  def createSource(delta: Vector)(implicit source: Source): Source = {
    val (xs, ls) = delta.zipWithIndex.map{ case (value, id) => value + source(id) }.splitAt(xParamsAmount)
    Solver.createSource(xs, ls)
  }

  def shouldStop(vector: Vector) = vector.forall(math.abs(_) < epsilon)
}
