package es.eriktorr
package shopping_cart

import commons.spec.StringGenerators.alphaLowerStringBetween
import shopping_cart.Product.{Cost, Name, Revenue, Tax}

import org.scalacheck.Gen

object ProductGenerators:
  val nameGen: Gen[Name] = alphaLowerStringBetween(3, 7).map(Name.applyUnsafe)

  private val costGen = Gen.choose(1d, 10_100d).map(Cost.applyUnsafe)

  private val revenueGen = Gen.choose(1, 100).map(Revenue.applyUnsafe)

  private val taxGen = Gen.choose(1, 100).map(Tax.applyUnsafe)

  def productGen(
      nameGen: Gen[Name] = nameGen,
      costGen: Gen[Cost] = costGen,
      revenueGen: Gen[Revenue] = revenueGen,
      taxGen: Gen[Tax] = taxGen,
  ): Gen[Product] =
    for
      name <- nameGen
      cost <- costGen
      revenue <- revenueGen
      tax <- taxGen
    yield Product(name, cost, revenue, tax)
