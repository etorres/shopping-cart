package es.eriktorr
package shopping_cart

import commons.spec.StringGenerators.alphaLowerStringBetween
import shopping_cart.Coupon.{Code, Discount}

import org.scalacheck.Gen

object CouponGenerators:
  val codeGen: Gen[Code] = alphaLowerStringBetween(3, 7).map(Code.applyUnsafe)

  private val discountGen = Gen.choose(1, 100).map(Discount.applyUnsafe)

  def couponGen(
      codeGen: Gen[Code] = codeGen,
      discountGen: Gen[Discount] = discountGen,
  ): Gen[Coupon] =
    for
      code <- codeGen
      discount <- discountGen
    yield Coupon(code, discount)
