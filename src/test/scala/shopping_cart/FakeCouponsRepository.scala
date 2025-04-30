package es.eriktorr
package shopping_cart

import cats.data.OptionT
import cats.effect.IO
import cats.effect.std.MapRef

final class FakeCouponsRepository private (mapRef: MapRef[IO, Coupon.Code, Option[Coupon]])
    extends CouponsRepository:
  override def add(coupon: Coupon): IO[Unit] =
    IO.raiseError(IllegalAccessError("Cannot modify a read-only repository"))

  override def findCouponBy(code: Coupon.Code): OptionT[IO, Coupon] =
    OptionT:
      mapRef(code).get

object FakeCouponsRepository:
  def fillWith(coupons: Map[Coupon.Code, Coupon]): IO[FakeCouponsRepository] =
    for
      mapRef <- MapRef.ofSingleImmutableMap[IO, Coupon.Code, Coupon](coupons)
      couponsRepository = FakeCouponsRepository(mapRef)
    yield couponsRepository
