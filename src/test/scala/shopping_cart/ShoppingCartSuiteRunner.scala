package es.eriktorr
package shopping_cart

import commons.spec.FakeConsole
import commons.spec.FakeConsole.ConsoleState

import cats.effect.std.Console
import cats.effect.{IO, Ref}

object ShoppingCartSuiteRunner:
  final case class ShoppingCartSuiteState(
      consoleState: ConsoleState,
      coupons: Map[Coupon.Code, Coupon],
      products: Map[Product.Name, Product],
  ):
    def setCoupons(coupons: Map[Coupon.Code, Coupon]): ShoppingCartSuiteState =
      copy(coupons = coupons)

    def setLines(lines: List[String]): ShoppingCartSuiteState =
      copy(consoleState = consoleState.set(lines))

    def setProducts(products: Map[Product.Name, Product]): ShoppingCartSuiteState =
      copy(products = products)

  object ShoppingCartSuiteState:
    val empty: ShoppingCartSuiteState = ShoppingCartSuiteState(
      ConsoleState.empty,
      Map.empty,
      Map.empty,
    )

  def runWith[A](
      initialState: ShoppingCartSuiteState,
  )(run: ShoppingCart => IO[A]): IO[(Either[Throwable, A], ShoppingCartSuiteState)] =
    for
      consoleStateRef <- Ref.of[IO, ConsoleState](initialState.consoleState)
      couponsRepository <- FakeCouponsRepository.fillWith(initialState.coupons)
      productsRepository <- FakeProductsRepository.fillWith(initialState.products)
      shoppingCart <-
        given Console[IO] = FakeConsole(consoleStateRef)
        ShoppingCart.InMemory.impl(couponsRepository, productsRepository)
      result <- run(shoppingCart).attempt
      finalConsoleState <- consoleStateRef.get
      finalState = initialState.copy(
        consoleState = finalConsoleState,
      )
      _ = result match
        case Left(error) => error.printStackTrace()
        case _ => ()
    yield result -> finalState
