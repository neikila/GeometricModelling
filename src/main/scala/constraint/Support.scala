package constraint

/**
  * Created by k.neyman on 18.01.2017.
  */
trait Support {
  implicit class Power(value: Double) {
    def ^(power: Double) = math.pow(value, power)
    def ^(power: Int) = math.pow(value, power)
  }
}
