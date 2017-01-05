package solver

import solver.Solver.{Matrix, Vector}

import scala.language.postfixOps

/**
  * Created by k.neyman on 04.01.2017.
  */
class Gaus(val matrix: Matrix, val right: Vector) extends Solver {
  require(matrix.size > 1, "Empty matrix")

  lazy val result = solve()

  def solve(): AB = back(straight(AB(matrix, right), 0))

  private def straight(ab: AB, index: Int): AB = {
    val matrix = ab.A
    val B = ab.B

    val base = matrix.head
    val diagonal: Double = base(index)

    if (diagonal == 0) return straight(ab.sendToEnd, index)

    assert(diagonal != 0, "diagonal == 0")

    val reduced = base.map(_ / diagonal)
    val bReduced = B.head / diagonal

    val temp = (matrix.drop(1) zip B.drop(1)) map {
      case (vector, b) =>
        val head = vector(index)
        (vector.toStream zip reduced map { case (a, c) => a - head * c } toIndexedSeq,
          b - head * bReduced)
    }

    if (temp.nonEmpty) {
      val (newA, newB) = temp.unzip
      new AB(reduced, bReduced, straight(AB(newA, newB), index + 1))
    }
    else AB(IndexedSeq(reduced), IndexedSeq(bReduced))
  }

  private def back(ab: AB): AB = straight(ab.reverse, 0).reverse
}

case class AB(A: Matrix, B: Vector) {
  def this(headVector: Vector, b: Double, ab: AB) {
    this(headVector +: ab.A, b +: ab.B)
  }

  override def toString: String = A zip B mkString "\n"

  def reverse = AB(
    A.map(_.reverse).reverse,
    B.reverse
  )

  def sendToEnd = AB(
    A match { case (head +: tail) => tail :+ head },
    B match { case (head +: tail) => tail :+ head }
  )
}
