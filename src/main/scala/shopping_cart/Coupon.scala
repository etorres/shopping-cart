package es.eriktorr
package shopping_cart

import commons.refined.Constraints.{Percentage, ValidCode}
import commons.validated.EitherNecExtensions.AllErrorsOr

import cats.implicits.catsSyntaxTuple2Parallel
import io.github.iltotore.iron.RefinedType
import io.github.iltotore.iron.cats.*

final case class Coupon(code: Coupon.Code, discount: Coupon.Discount)

object Coupon:
  type Code = Code.T
  object Code extends RefinedType[String, ValidCode]

  type Discount = Discount.T
  object Discount extends RefinedType[Int, Percentage]

  private def couponEitherNec(code: String, discount: Int): AllErrorsOr[Coupon] =
    (
      Code.eitherNec(code),
      Discount.eitherNec(discount),
    ).parMapN(Coupon.apply)

  def promotion(discount: Int): AllErrorsOr[Coupon] =
    couponEitherNec(s"PROMO_$discount", discount)
