package es.eriktorr
package shopping_cart

import cats.data.OptionT
import cats.effect.IO

final class FakeCouponsRepository(coupons: Map[Coupon.Code, Coupon]) extends CouponsRepository:
  override def add(coupon: Coupon): IO[Unit] =
    IO.raiseError(IllegalAccessError("Cannot modify a read-only repository"))

  override def findCouponBy(code: Coupon.Code): OptionT[IO, Coupon] =
    OptionT:
      IO.pure(coupons.get(code))
