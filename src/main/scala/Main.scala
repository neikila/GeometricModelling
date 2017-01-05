import constraint.{Axis, FixedAxis}
import form.Point
import solver.{AB, Gaus, MatrixBuilder, ResultExtractor}

import scala.language.postfixOps

/**
  * Created by k.neyman on 04.01.2017.
  */

object Main {
  def main(args: Array[String]): Unit = {
    println(ABFromBuilder)

    println("\nResult:")

    val result = new ResultExtractor(new Gaus(ABFromBuilder).result, 2).extract
    println(result.toList)
  }

  def ABFromBuilder = {
    val p1 = Point(10, 20)
    val p2 = Point(3, 3)
    new MatrixBuilder(
      FixedAxis(Axis.X, 20, p1) :: FixedAxis(Axis.Y, 13, p1) :: Nil,
      p1 :: p2 :: Nil
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
