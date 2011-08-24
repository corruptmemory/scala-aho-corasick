package com.corruptmemory.aho_corasick

import org.scalacheck._
import Gen._

object AhoCorasickSpecification extends Properties("Aho-Corasick") {
  import AhoCorasickBuilder._

  // Reasonable length strings
  val stringGen = for {
    n <- choose(8,20)
    str <- (for (cs <- listOfN(n,alphaNumChar)) yield cs.mkString)
    } yield str
  val stringListGen = containerOf[List,String](stringGen)

  property("build trie/find values") = Prop.forAll(stringListGen) {
    (l:List[String]) => {
      val ac = AhoCorasickBuilder(l.map(Data(_,())))
      val finder = ac.build()
      l.forall(str => finder.find(str).forall( a => (a.actual == str) || a.actual.contains(str)))
    }
  }
}
