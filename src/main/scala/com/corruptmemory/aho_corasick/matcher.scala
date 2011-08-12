package com.corruptmemory.aho_corasick

import scalaz._
import Scalaz._
import scala.collection.mutable.{Map => MMap, Set => MSet, Queue => MQueue}

object Matcher {
  type StateID = Int
  type OutputMap = MMap[StateID,MSet[String]]
  type FailureMap = MMap[StateID,Goto]

  case class NextMove(successMap:MMap[StateID,MMap[Char,Goto]] = MMap[StateID,MMap[Char,Goto]](),fail:MMap[StateID,Goto] = MMap[StateID,Goto]()) {
    def +=(x:(StateID,(Char,Goto))) = {
      successMap.get(x._1).fold(some = s => s += x._2,
                                none = successMap += x._1 -> MMap(x._2))
      this
    }
  }

  trait Goto {
    val nextID:() => StateID
    val id:StateID = nextID()
    val values:MMap[Char,Goto]
    def apply(c:Char):Option[Goto]
    def +=(x:(Char,Goto)):Goto = {
      values += x
      x._2
    }
    def <<(c:Char):Goto = {
      values.get(c).fold(none = this += (c -> BodyGoto(nextID)),
                         some = s => s)
    }
  }

  case class BodyGoto(nextID:() => StateID,values:MMap[Char,Goto] = MMap[Char,Goto]()) extends Goto {
    def apply(c:Char):Option[Goto] = values.get(c)
  }

  case class RootGoto(nextID:() => StateID,values:MMap[Char,Goto] = MMap[Char,Goto]()) extends Goto {
    def apply(c:Char):Option[Goto] = values.get(c) orElse (some(this))
  }

  class StateIDGen {
    var currentID:StateID = 0
    def nextID():StateID = {
      val r = currentID
      currentID += 1
      r
    }
  }

  def enter(outputs:Seq[String],charMap:Char => Char = _.toLowerCase):(Goto,OutputMap) = {
    val idGen = new StateIDGen
    val rootGoto:Goto = RootGoto(nextID = idGen.nextID _)
    val outputMap:OutputMap = MMap[StateID,MSet[String]]()
    def innerEnter(output:String) {
      val goto = output.map(charMap(_)).foldLeft(rootGoto)((s,c) => s << c)
      outputMap.get(goto.id).fold(none = outputMap += goto.id -> MSet(output),
                                  some = s => outputMap += goto.id -> (s += output))
    }
    outputs.foreach(innerEnter(_))
    (rootGoto,outputMap)
  }

  def computeFailure(rootGoto:Goto,outputMap:OutputMap):(FailureMap,OutputMap) = {
    val failureMap:FailureMap = MMap[StateID,Goto]()
    val queue = MQueue[Goto]()
    rootGoto.values.foreach {
      case (a:Char,s:Goto) => {
        failureMap += s.id -> rootGoto
        queue += s
      }
    }
    while (!queue.isEmpty) {
      val r = queue.dequeue()
      r.values.foreach {
        case(a:Char,s:Goto) => {
          queue += s
          var state = failureMap(r.id)
          while (!state(a).isDefined) {
            state = failureMap(state.id)
          }
          val down = state(a).get
          failureMap += s.id -> down
          outputMap.get(down.id).foreach {
            dos => {
              outputMap.get(s.id).fold(none = outputMap += s.id -> dos,
                                       some = s1 => outputMap += s.id -> (s1 ++ dos))
            }
          }
        }
      }
    }
    (failureMap,outputMap)
  }

  def computeDFA(rootGoto:Goto,failureMap:FailureMap):NextMove = {
    val nextMove:NextMove = NextMove()
    val queue = MQueue[Goto]()
    rootGoto.values.foreach {
      case (c,g) => {
        nextMove += (0 -> (c -> g))
        queue += g
      }
    }
    while (!queue.isEmpty) {
      val r = queue.dequeue()
      r.values.foreach {
        case (a,s) => {
          queue += s
          nextMove += (r.id -> (a -> s))
        }
      }
      nextMove.fail += (r.id -> failureMap(r.id))
    }
    nextMove
  }
}
