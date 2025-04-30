package es.eriktorr
package commons.validated

import cats.data.{EitherNec, NonEmptyChain}

object EitherNecExtensions:
  type AllErrorsOr[A] = EitherNec[String, A]

  extension [A](self: AllErrorsOr[A])
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def orFail: A = self match
      case Left(errors) => throw throwableFrom(errors)
      case Right(value) => value

    private def throwableFrom(errors: NonEmptyChain[String]) =
      val errorList = errors.toNonEmptyList.toList.mkString("[", ",", "]")
      IllegalArgumentException(s"Validation errors: $errorList")
