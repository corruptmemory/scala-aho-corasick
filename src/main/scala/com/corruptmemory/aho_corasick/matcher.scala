package com.corruptmemory.aho_corasick

import scalaz._
import Scalaz._
import scala.collection.mutable.{Map => MMap, Set => MSet, Queue => MQueue}

class AhoCorasick(charMap:Char => Char = _.toLower) {
  import AhoCorasick._
  var currentID:StateID = 0
  val rootGoto:Goto = new Goto with RootGoto

  def nextID():StateID = {
    val r  = currentID
    currentID += 1
    r
  }

  trait RootGoto {
    self:Goto =>
    override def goto(c:Char):Option[Goto] = next.get(c) orElse (some(self))
  }

  class Goto {
    val id:StateID = nextID()
    val next:MMap[Char,Goto] = MMap[Char,Goto]()
    var outputs:Option[MSet[String]] = none
    var fail:Option[Goto] = none
    def goto(c:Char):Option[Goto] = next.get(c)
  }

  def +=(in:String):AhoCorasick = {
    val target = in.map(charMap(_)).foldLeft(rootGoto) {
      (g,c) => {
        g.next.get(c).fold(none = {
                             val n = new Goto
                             g.next += c -> n
                             n
                           },
                           some = s => s)
      }
    }
    target.outputs.fold(none = target.outputs = some(MSet(in)),
                        some = s => s += in)
    this
  }

  def build():AhoCorasick = {
    val queue = MQueue[Goto]()
    rootGoto.next.values.foreach {
      (s:Goto) => {
        s.fail = some(rootGoto)
        queue += s
      }
    }
    while (!queue.isEmpty) {
      val r = queue.dequeue()
      r.next.foreach {
        case(a:Char,s:Goto) => {
          queue += s
          var state = r.fail.get
          while (!state.goto(a).isDefined) {
            state = state.fail.get
          }
          val down = state.goto(a).get
          s.fail = some(down)
          down.outputs.foreach {
            dos => {
              s.outputs.fold(none = s.outputs = some(dos),
                             some = s1 => s.outputs = some(s1 ++= dos))
            }
          }
        }
      }
    }
    this
  }

  def find(in:String):Seq[Match] = {
    var state = rootGoto
    val builder = Vector.newBuilder[Match]
    in.map(charMap(_)).zipWithIndex.foreach {
      case (c,i) => {
        while (!state.goto(c).isDefined) { state = state.fail.get }
        state = state.goto(c).get
        state.outputs.foreach {
          s => {
            builder ++= s.toSeq.map(x => Match(i-x.length+1,x,in.slice(i-x.length+1,i+1)))
          }
        }
      }
    }
    builder.result
  }

  def countOutputs():Int = {
    var cnt = 0
    def internalCO(g:Goto) {
      g.outputs.foreach { s=> cnt += s.size }
      g.next.values.foreach(internalCO(_))
    }
    internalCO(rootGoto)
    cnt
  }

  def dumpOutputs() {
    def internalDO(g:Goto) {
      g.outputs.foreach { s=>
        println("%d:%s".format(g.id,s))
      }
      g.next.values.foreach(internalDO(_))
    }
    internalDO(rootGoto)
  }
}

object AhoCorasick {
  type StateID = Int
  def apply(in:Seq[String]):AhoCorasick =
    in.foldLeft(new AhoCorasick())((s,v) => s += v)
}