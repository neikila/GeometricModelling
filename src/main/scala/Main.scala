import constraint.{Axis, FixedAxis}
import form.Point
import solver.{AB, Gaus, MatrixBuilder}

import scala.language.postfixOps

/**
  * Created by k.neyman on 04.01.2017.
  */

object Main {
  def main(args: Array[String]): Unit = {
    println(ABTemp)

    println()
    println(ABFromBuilder)

    println("\nResult:")
    println(new Gaus(ABFromBuilder).result)
  }

  def ABFromBuilder = {
    val p = Point(10, 20)
    new MatrixBuilder(
      FixedAxis(Axis.Y, 13, p) :: FixedAxis(Axis.X, 20, p) :: Nil,
      p :: Nil
    ).build()
  }

//  (Vector(2.0, 0.0, 0.0, -1.0),20.0)
//  (Vector(0.0, 2.0, -1.0, 0.0),40.0)
//  (Vector(0.0, 1.0, 0.0, 0.0),13.0)
//  (Vector(1.0, 0.0, 0.0, 0.0),20.0)

  def ABTemp = {
    val matrixSize = 4

    val (matrix, right) = IndexedSeq[Double](
      2, 0, 0, -1, 2 * 10,
      0, 2, -1, 0, 2 * 20,
      0, 1, 0, 0, 13,
      1, 0, 0, 0, 20
    ).sliding(matrixSize + 1, matrixSize + 1).map { vector =>
      vector.splitAt(matrixSize) match {
        case (a, b) => (a, b.head)
      }
    }.toIndexedSeq.unzip

    AB(matrix, right)
  }
}
