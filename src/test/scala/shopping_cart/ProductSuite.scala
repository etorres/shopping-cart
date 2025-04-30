package es.eriktorr
package shopping_cart

import commons.validated.EitherNecExtensions.orFail
import shopping_cart.Product.{Cost, Name, Revenue}

import munit.FunSuite

final class ProductSuite extends FunSuite:
  test("should calculate the price of the iceberg lettuce"):
    val product = Product.normal(Name("Iceberg"), Cost(1.55d), Revenue(15)).orFail
    assertEquals(product.unitPrice, 1.79d, "unit price")
    assertEquals(product.finalPrice, 2.17d, "final price")

  test("should calculate the price of the tomato"):
    val product = Product.normal(Name("Tomato"), Cost(0.52d), Revenue(15)).orFail
    assertEquals(product.unitPrice, 0.60d, "unit price")
    assertEquals(product.finalPrice, 0.73d, "final price")

  test("should calculate the price of the chicken"):
    val product = Product.normal(Name("Chicken"), Cost(1.34d), Revenue(12)).orFail
    assertEquals(product.unitPrice, 1.51d, "unit price")
    assertEquals(product.finalPrice, 1.83d, "final price")

  test("should calculate the price of the bread"):
    val product = Product.firstNecessity(Name("Bread"), Cost(0.71d), Revenue(12)).orFail
    assertEquals(product.unitPrice, 0.80d, "unit price")
    assertEquals(product.finalPrice, 0.88d, "final price")

  test("should calculate the price of the corn"):
    val product = Product.firstNecessity(Name("Corn"), Cost(1.21d), Revenue(12)).orFail
    assertEquals(product.unitPrice, 1.36d, "unit price")
    assertEquals(product.finalPrice, 1.50d, "final price")
