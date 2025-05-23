package es.eriktorr
package commons.spec

import org.scalacheck.Gen

object StringGenerators:
  def alphaLowerStringBetween(minLength: Int, maxLength: Int): Gen[String] =
    stringBetween(minLength, maxLength, Gen.alphaLowerChar)

  private def stringBetween(minLength: Int, maxLength: Int, charGen: Gen[Char]): Gen[String] =
    for
      stringLength <- Gen.choose(minLength, maxLength)
      string <- stringOfLength(stringLength, charGen)
    yield string

  private def stringOfLength(length: Int, charGen: Gen[Char]): Gen[String] =
    Gen.listOfN(length, charGen).map(_.mkString)
