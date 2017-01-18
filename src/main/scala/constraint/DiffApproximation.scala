package constraint
import solver.Solver.Source

/**
  * Created by k.neyman on 18.01.2017.
  */
trait DiffApproximation extends Differentiable {
  val delta = 0.00001

  def sourcePair(source: Source, variable: Var) = {
    val s1: Source = (v: Var) => {
      if (v == variable) source(v) - delta
      else source(v)
    }
    val s2: Source = (v: Var) => {
      if (v == variable) source(v) + delta
      else source(v)
    }
    (s1, s2)
  }

  override def diffBy(diffBy: Var)(implicit source: Source): Double = {
    val (s1, s2) = sourcePair(source, diffBy)
    (f(s2) - f(s1)) / (2 * delta)
  }

  override def diffBy(diffBy1: Var, diffBy2: Var)(implicit source: Source): Double = {
    val (s1, s2) = sourcePair(source, diffBy2)
    (diffBy(diffBy1)(s2) - diffBy(diffBy2)(s1)) / (2 * delta)
  }
}
