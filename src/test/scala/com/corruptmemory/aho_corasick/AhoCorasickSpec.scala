/**
 * AhoCorasickSpec.scala
 *
 * @author <a href="mailto:jim@corruptmemory.com">Jim Powers</a>
 *
 * Copyright 2011 Jim Powers
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
