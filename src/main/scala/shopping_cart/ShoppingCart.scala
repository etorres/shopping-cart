package es.eriktorr
package shopping_cart

import commons.error.HandledError
import commons.math.Arithmetics.roundedUp
import shopping_cart.ShoppingCart.ShoppingCart.{
  CouponAlreadyAppliedError,
  CouponNotFoundError,
  ProductNotFoundError,
}

import cats.data.OptionT
import cats.effect.std.{Console, MapRef}
import cats.effect.{IO, Ref}
import cats.implicits.toTraverseOps

import java.util.concurrent.ConcurrentHashMap as JConcurrentHashMap

trait ShoppingCart:
  def addProductBy(name: Product.Name): OptionT[IO, Double]
  def applyCoupon(code: Coupon.Code): OptionT[IO, Coupon.Discount]
  def deleteProductBy(name: Product.Name): OptionT[IO, Double]
  def printStatus(): IO[Unit]

object ShoppingCart:
  final class InMemory private (
      couponsRepository: CouponsRepository,
      productsRepository: ProductsRepository,
      productsJMap: JConcurrentHashMap[Product.Name, Int],
      productsMapRef: MapRef[IO, Product.Name, Option[Int]],
      promotionRef: Ref[IO, Option[Coupon.Code]],
  )(using console: Console[IO])
      extends ShoppingCart:
    override def addProductBy(name: Product.Name): OptionT[IO, Double] =
      for
        product <- productsRepository.findProductBy(name)
        _ <- OptionT.liftF:
          productsMapRef(name).update: quantity =>
            quantity.map(_ + 1).orElse(Some(1))
      yield product.finalPrice

    override def applyCoupon(code: Coupon.Code): OptionT[IO, Coupon.Discount] =
      for
        coupon <- couponsRepository.findCouponBy(code)
        discount <- OptionT.liftF:
          promotionRef.flatModify:
            case Some(previousCode) =>
              Some(previousCode) -> IO.raiseError(CouponAlreadyAppliedError(previousCode))
            case None =>
              Some(code) -> IO.pure(coupon.discount)
      yield discount

    override def deleteProductBy(name: Product.Name): OptionT[IO, Double] =
      for
        product <- productsRepository.findProductBy(name)
        price <- OptionT.liftF:
          productsMapRef(name).modify:
            case Some(quantity) if quantity > 1 =>
              Some(quantity - 1) -> product.finalPrice
            case _ => None -> 0d
      yield -price

    override def printStatus(): IO[Unit] =
      for
        _ <- console.println("Product name | Price with VAT | Quantity")
        productNames <- productNames()
        accumulated <- productNames.sorted.traverse: name =>
          for
            product <- productsRepository
              .findProductBy(name)
              .getOrRaise(ProductNotFoundError(name))
            (quantity, finalPrice) <- productsMapRef(name).get
              .map(_.getOrElse(0))
              .flatMap: quantity =>
                if quantity > 0
                then
                  console.println(s"$name | ${product.finalPrice} € | $quantity") *>
                    IO.pure(quantity -> product.finalPrice)
                else IO.pure(0 -> 0d)
          yield quantity -> finalPrice
        _ <- promotionRef.get.map:
          case Some(code) =>
            for
              coupon <- couponsRepository
                .findCouponBy(code)
                .getOrRaise(CouponNotFoundError(code))
              _ <- console.println(s"Promotion: ${coupon.discount}% off with code $code")
            yield ()
          case None => IO.unit
        (totalProducts, totalPrice) =
          accumulated.fold(0 -> 0d) { case ((x1, y1), (x2, y2)) =>
            (x1 + x2) -> (y1 + y2)
          }
        _ <- console.println(s"Total products: $totalProducts")
        _ <- console.println(s"Total price: ${totalPrice.roundedUp(2)} €")
      yield ()

    private def productNames() =
      import scala.jdk.CollectionConverters.*
      IO.delay(productsJMap.keys()).map(_.asScala.toList)

  object InMemory:
    def impl(
        couponsRepository: CouponsRepository,
        productsRepository: ProductsRepository,
    )(using console: Console[IO]): IO[InMemory] =
      for
        concurrentHashMap <- IO.pure(JConcurrentHashMap[Product.Name, Int]())
        mapRef = MapRef.fromConcurrentHashMap[IO, Product.Name, Int](concurrentHashMap)
        promotionRef <- Ref.of[IO, Option[Coupon.Code]](None)
      yield InMemory(
        couponsRepository,
        productsRepository,
        concurrentHashMap,
        mapRef,
        promotionRef,
      )

  abstract class ShoppingCartError(message: String) extends HandledError(message)

  object ShoppingCart:
    final case class CouponAlreadyAppliedError(code: Coupon.Code)
        extends ShoppingCartError(s"A coupon code was already applied: $code")

    final case class CouponNotFoundError(code: Coupon.Code)
        extends ShoppingCartError(s"Coupon not found: $code")

    final case class ProductNotFoundError(name: Product.Name)
        extends ShoppingCartError(s"Product not found: $name")
