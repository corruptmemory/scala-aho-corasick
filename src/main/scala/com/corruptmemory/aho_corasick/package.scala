package com.corruptmemory

package object aho_corasick {
  import scalaz._
  import Scalaz._

  type Matches = Option[NonEmptyList[Match]]
}
