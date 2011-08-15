package com.corruptmemory.aho_corasick

import scalaz._
import Scalaz._
import scala.collection.mutable.{Set => MSet, Queue => MQueue}

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
    override def goto(c:Char):Option[Goto] = next.get(c).map(_.data) orElse (some(self))
  }

  class Goto {
    val id:StateID = nextID()
    val next:Node[Goto] = Node(this)
    var outputs:Option[MSet[String]] = none
    var fail:Option[Goto] = none
    def goto(c:Char):Option[Goto] = next.get(c).map(_.data)
    def failToString:String = {
      fail.fold(none = "<>",
                some = s => "<"+s.toString+">")
    }
    override def toString:String = {
      "goto(%d,%s,%s,%s)".format(id,outputs,failToString,next.entries.map(_.map(neToString(_)).mkString("[",",","]")))
    }
  }

  def +=(in:String):AhoCorasick = {
    val target = in.map(charMap(_)).foldLeft(rootGoto) {
      (g,c) => {
        g.next.get(c).fold(none = {
                             val n = (new Goto).next
                             g.next += c -> n
                             n.data
                           },
                           some = s => s.data)
      }
    }
    target.outputs.fold(none = target.outputs = some(MSet(in)),
                        some = s => s += in)
    this
  }

  def neToString(ne:NodeEntry[Goto]):String =
    "{%s: %s}".format(ne.char,ne.node.data.toString)

  def debugNE(ne:NodeEntry[Goto]) {
    println(neToString(ne))
  }

  def debugN(n:Node[Goto]) {
    println(n.data.toString)
  }

  def build():AhoCorasick = {
    val queue = MQueue[Goto]()
    rootGoto.next.entries.get.foreach {
      (s:NodeEntry[Goto]) => {
        // debugNE(s)
        s.node.data.fail = some(rootGoto)
        queue += s.node.data
      }
    }
    while (!queue.isEmpty) {
      val r = queue.dequeue()
      r.next.entries.foreach {
        _.foreach {
          (xx:NodeEntry[Goto]) => {
            // debugNE(xx)
            val a:Char = xx.char
            val s:Goto = xx.node.data
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
}

object AhoCorasick {
  type StateID = Int
  def apply(in:Seq[String]):AhoCorasick =
    in.foldLeft(new AhoCorasick())((s,v) => s += v)
}