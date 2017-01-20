package solver

import solver.Solver.{Matrix, Vector}

import scala.language.postfixOps

/**
  * Created by k.neyman on 04.01.2017.
  */
class Gaus(val ab: AB) extends Solver {
  require(ab.A.size > 1, "Empty matrix")
  def this(matrix: Matrix, right: Vector) = this(AB(matrix, right))

  lazy val result = solve()

  def solve(): AB = back(straight(ab, 0))

  private def straight(ab: AB, index: Int): AB = {
    val AB(matrix, b) = ab.correct(index)

    val base = matrix.head
    val diagonal: Double = base(index)

    val reduced = base.map(_ / diagonal)
    val bReduced = b.head / diagonal

    val temp = (matrix.drop(1) zip b.drop(1)).par.map({
      case (vector, bVal) =>
        val head = vector(index)
        if (head != 0)
          (vector.par zip reduced map { case (a, c) => a - head * c } toIndexedSeq,
            bVal - head * bReduced)
        else (vector, bVal)
    }).toIndexedSeq

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

  def correct(index: Int) = {
    if (A.head(index) != 0) this
    else if (A.tail.forall(line => line(index) == 0) && A.head.forall(_ == 0)) {
      println("Boom!")
      AB(A match { case (head +: tail) => head.updated(index, 1.0) +: tail }, B)
    }
    else sendToEnd
  }

  def sendToEnd = AB(
    A match { case (head +: tail) => tail :+ head },
    B match { case (head +: tail) => tail :+ head }
  )

  def ++(ab: AB) = AB(A ++ ab.A, B ++ ab.B)
}
