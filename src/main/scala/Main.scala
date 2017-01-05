import solver.Gaus

import scala.language.postfixOps

/**
  * Created by k.neyman on 04.01.2017.
  */

object Main {
  def main(args: Array[String]): Unit = {
    val (matrix, right) = AB.unzip
    println(AB.mkString("\n"))

    println("\nResult:")
    println(new Gaus(matrix, right).result)
  }

  def AB = {
    val matrixSize = 2

    IndexedSeq[Double](
      0, 4,   2,
      2, 2,   5
    ).sliding(matrixSize + 1, matrixSize + 1).map { vector =>
      vector.splitAt(matrixSize) match { case (a, b) => (a, b.head)}
    } toIndexedSeq
  }
}
