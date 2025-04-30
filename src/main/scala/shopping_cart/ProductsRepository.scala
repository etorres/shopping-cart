package es.eriktorr
package shopping_cart

import cats.data.OptionT
import cats.effect.IO

trait ProductsRepository:
  def add(product: Product): IO[Unit]
  def findProductBy(name: Product.Name): OptionT[IO, Product]
