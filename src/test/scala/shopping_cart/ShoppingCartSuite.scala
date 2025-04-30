package es.eriktorr
package shopping_cart

import commons.math.Arithmetics.roundedUp
import commons.spec.CollectionGenerators.nDistinct
import shopping_cart.CouponGenerators.couponGen
import shopping_cart.ProductGenerators.{nameGen, productGen}
import shopping_cart.ShoppingCartSuite.{testCaseGen, TestCase}
import shopping_cart.ShoppingCartSuiteRunner.{runWith, ShoppingCartSuiteState}

import cats.effect.IO
import cats.implicits.{toFoldableOps, toTraverseOps}
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.cats.implicits.genInstances
import org.scalacheck.effect.PropF.forAllF

final class ShoppingCartSuite extends CatsEffectSuite with ScalaCheckEffectSuite:
  test("should list a shopping cart"):
    forAllF(testCaseGen):
      case TestCase(names, code, initialState, expectedFinalState) =>
        runWith(initialState): shoppingCart =>
          for
            _ <- names.traverse_(shoppingCart.addProductBy(_).value)
            _ <- shoppingCart.applyCoupon(code).value
            _ <- shoppingCart.printStatement()
          yield ()
        .map:
          case (result, obtainedFinalState) =>
            assert(result.isRight)
            assertEquals(obtainedFinalState, expectedFinalState)

object ShoppingCartSuite:
  final private case class TestCase(
      names: List[Product.Name],
      code: Coupon.Code,
      initialState: ShoppingCartSuiteState,
      finalState: ShoppingCartSuiteState,
  )

  private val testCaseGen = for
    names <- nDistinct(5, nameGen)
    products <- names.traverse(name => productGen(nameGen = name))
    size <- Gen.choose(7, 11)
    selectedProducts <- Gen.listOfN(size, Gen.oneOf(products))
    coupon <- couponGen()
    expectedLines = shoppingCartListFrom(selectedProducts)
    initialState = ShoppingCartSuiteState.empty
      .setCoupons(Map(coupon.code -> coupon))
      .setProducts(products.map(product => product.name -> product).toMap)
    finalState = initialState.setLines(expectedLines)
  yield TestCase(selectedProducts.map(_.name), coupon.code, initialState, finalState)

  private def shoppingCartListFrom(products: List[Product]) =
    val accumulated = products
      .groupBy(_.name)
      .toList
      .sortBy:
        case (name, _) => name
      .map:
        case (name, items) =>
          val finalPrice = items.headOption.map(_.finalPrice).getOrElse(0d)
          val quantity = items.length
          (name, finalPrice, quantity)
    val totalPrice = accumulated
      .map:
        case (_, finalPrice, _) => finalPrice
      .sum
    val totalProducts = products.length
    val lines = accumulated.map:
      case (name, finalPrice, quantity) => s"$name | $finalPrice € | $quantity"
    ("Product name | Price with VAT | Quantity" :: lines ++ List(
      s"Total products: $totalProducts",
      s"Total price: ${totalPrice.roundedUp(2)} €",
    )).reverse
