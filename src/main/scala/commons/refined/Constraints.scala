package es.eriktorr
package commons.refined

import io.github.iltotore.iron.constraint.any.Not
import io.github.iltotore.iron.constraint.collection.{MaxLength, MinLength}
import io.github.iltotore.iron.constraint.numeric.Interval.Closed
import io.github.iltotore.iron.constraint.string.{Blank, Trimmed}

object Constraints:
  type Percentage = Closed[1, 100]

  type ValidCode = Trimmed & Not[Blank] & MinLength[3] & MaxLength[64]

  type ValidName = Trimmed & Not[Blank] & MinLength[3] & MaxLength[64]
