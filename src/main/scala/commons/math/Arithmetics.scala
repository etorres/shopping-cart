package es.eriktorr
package commons.math

object Arithmetics:
  extension (self: Double)
    def incrementedBy(percentage: Int): Double =
      (self * percentage / 100d) + self

    def roundedUp(decimals: Int): Double =
      BigDecimal(self)
        .setScale(decimals, BigDecimal.RoundingMode.UP)
        .toDouble
