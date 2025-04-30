package es.eriktorr
package shopping_cart

import cats.data.OptionT
import cats.effect.IO

trait CouponsRepository:
  def add(coupon: Coupon): IO[Unit]
  def findCouponBy(code: Coupon.Code): OptionT[IO, Coupon]
