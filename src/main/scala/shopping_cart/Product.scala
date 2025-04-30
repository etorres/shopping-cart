package es.eriktorr
package shopping_cart

import commons.math.Arithmetics.{incrementedBy, roundedUp}
import commons.refined.Constraints.{Percentage, ValidName}
import commons.validated.EitherNecExtensions.AllErrorsOr

import cats.implicits.catsSyntaxTuple4Parallel
import io.github.iltotore.iron.RefinedType
import io.github.iltotore.iron.cats.*
import io.github.iltotore.iron.constraint.numeric.Positive

final case class Product(
    name: Product.Name,
    cost: Product.Cost,
    revenue: Product.Revenue,
    tax: Product.Tax,
):
  lazy val unitPrice: Double =
    cost
      .incrementedBy(revenue)
      .roundedUp(2)

  lazy val finalPrice: Double =
    unitPrice
      .incrementedBy(tax)
      .roundedUp(2)

object Product:
  type Name = Name.T
  object Name extends RefinedType[String, ValidName]

  type Cost = Cost.T
  object Cost extends RefinedType[Double, Positive]

  type Revenue = Revenue.T
  object Revenue extends RefinedType[Int, Percentage]

  type Tax = Tax.T
  object Tax extends RefinedType[Int, Percentage]

  private def productEitherNec(
      name: String,
      cost: Double,
      revenue: Int,
      tax: Int,
  ): AllErrorsOr[Product] =
    (
      Name.eitherNec(name),
      Cost.eitherNec(cost),
      Revenue.eitherNec(revenue),
      Tax.eitherNec(tax),
    ).parMapN(Product.apply)

  def firstNecessity(
      name: String,
      cost: Double,
      revenue: Int,
  ): AllErrorsOr[Product] =
    productEitherNec(name, cost, revenue, 10)

  def normal(
      name: String,
      cost: Double,
      revenue: Int,
  ): AllErrorsOr[Product] =
    productEitherNec(name, cost, revenue, 21)
