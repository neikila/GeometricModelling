package solver

import constraint._
import form.{Form, Point}
import solver.Solver._

import scala.collection.immutable.IndexedSeq
import scala.language.postfixOps

/**
  * Created by k.neyman on 05.01.2017.
  */
class MatrixBuilder(val constraints: List[Constraint], val forms: List[Point]) {
  private val xParamAmount: Int = forms.size * 2
  val size = constraints.size + xParamAmount

  val vars = List.tabulate(xParamAmount) { Var(_, Var.X) } :::
    List.tabulate(constraints.size) { Var(_, Var.L) }
  val L = new LagrangeFunction(forms, constraints)

  val build = (for (i <- vars; j <- vars) yield (i, j)).sliding(size, size).toList

  def createAMatrix(implicit source: Source): Matrix =
    build map { _ map { case (diffBy1, diffBy2) => L.diffBy(diffBy1, diffBy2) } toIndexedSeq} toIndexedSeq

  def createBVector(implicit source: Source): Vector =
    vars map { -1 * L.diffBy(_) } toIndexedSeq

  def printGausAndHesse(diff: Differentiable)(implicit source: Source): Unit = {
    vars.foreach(v => println(s"$v = ${source(v)}"))
    println("Grad:")
    vars.foreach(variable => println(s"$variable: ${diff.diffBy(variable)}"))
    println("Hesse:")
    build.foreach { line =>
      line.foreach { case (v1, v2) => print(s"$v1$v2 = ${diff.diffBy(v1, v2)}; ") }
      println
    }
  }
}
