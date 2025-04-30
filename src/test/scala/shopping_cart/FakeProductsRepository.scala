package es.eriktorr
package shopping_cart

import cats.data.OptionT
import cats.effect.IO
import cats.effect.std.MapRef

final class FakeProductsRepository private (mapRef: MapRef[IO, Product.Name, Option[Product]])
    extends ProductsRepository:
  override def add(product: Product): IO[Unit] =
    IO.raiseError(IllegalAccessError("Cannot modify a read-only repository"))

  override def findProductBy(name: Product.Name): OptionT[IO, Product] =
    OptionT:
      mapRef(name).get

object FakeProductsRepository:
  def fillWith(products: Map[Product.Name, Product]): IO[FakeProductsRepository] =
    for
      mapRef <- MapRef.ofSingleImmutableMap[IO, Product.Name, Product](products)
      productsRepository = FakeProductsRepository(mapRef)
    yield productsRepository
