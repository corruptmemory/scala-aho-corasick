package com.corruptmemory.aho_corasick

import scalaz._
import Scalaz._

class Matcher

object Matcher {
  type Outputs = Option[NonEmptyList[String]]

  sealed trait Trie {
    self =>
    val id:Int = Tries.nextID()
    val values:Map[Char,Trie]
    val outputs:Outputs
    def copy(values:Map[Char,Trie] = self.values,outputs:Outputs = self.outputs):Trie
    def next(c:Char):Option[Trie]
    def outputs(c:Char):Outputs = next(c).flatMap(_.outputs)
    def +(x:Tuple2[Char,Trie]):Trie = self.copy(values = self.values + x)
    def <<(s:String):Trie = self.copy(outputs = self.outputs |+| some(s.pure[NonEmptyList]))
    def <<<(s:Outputs):Trie = self.copy(outputs = self.outputs |+| s)
    def ++(that:Trie):Trie = self.copy(values = self.values ++ that.values, outputs = self.outputs |+| that.outputs)
    def nextOrElse(c:Char,alt: => Trie):Trie = values.getOrElse(c,alt)
    def valuesToString:Seq[String] = {
      val vs = values.toSeq
      if (vs.isEmpty) Seq(outputsToString)
      else vs.map {
        case (k,v) => "%s->%s".format(k,outputs.fold(none = v.toString,some = _ => " <%s|%s>".format(outputsToString,v.toString)))
      }
    }
    def outputsToString:String = outputs.fold(none = "()",
                                              some = s => s.list.mkString("(",",",")"))
  }

  case class BodyTrie(values:Map[Char,Trie] = Map[Char,Trie](),outputs:Outputs = none) extends Trie {
    self =>
    def next(c:Char):Option[Trie] = values.get(c)
    def copy(values:Map[Char,Trie] = self.values,outputs:Outputs = self.outputs):Trie = BodyTrie(values,outputs)
    override def toString:String = "B[%d]:%s".format(id,valuesToString.mkString("{",",","}"))
  }

  case class RootTrie(values:Map[Char,Trie] = Map[Char,Trie](),outputs:Outputs = none) extends Trie {
    self =>
    def next(c:Char):Option[Trie] = values.get(c) orElse (some(this))
    def copy(values:Map[Char,Trie] = self.values,outputs:Outputs = self.outputs):Trie = RootTrie(values,outputs)
    override def toString:String = "R[%d]:%s".format(id,valuesToString.mkString("{",",","}"))
  }

  object Tries {
    var currentId = 0
    def nextID():Int = {
      val r = currentId
      currentId += 1
      return r
    } 
  }

  case class OutputsMap(values:Map[Trie,NonEmptyList[String]] = Map[Trie,NonEmptyList[String]]()) {
    self =>
    def +(x:Tuple2[Trie,Outputs]):OutputsMap = 
      x._2.fold(none = this,
                some = s => self.values.get(x._1).fold(none = OutputsMap(self.values + (x._1 -> s)),
                                                       some = s1 => OutputsMap(self.values + (x._1 -> (s1 |+| s)))))
  }

  case class FailureMap(values:Map[Trie,Trie] = Map[Trie,Trie]()) {
    self =>
    def apply(in:Trie):Trie = values(in)
    def get(in:Trie):Option[Trie] = values.get(in)
    def +(x:Tuple2[Trie,Trie]):FailureMap = self.copy(values = self.values + x)
    def ++(that:FailureMap):FailureMap = self.copy(values = self.values ++ that.values)
    def getOrElse(in:Trie,alt: => Trie):Trie = values.getOrElse(in,alt)
  }

  def apply(dictionary:Seq[String],charMapper:Char => Char = _.toLowerCase):Trie = {
    def trieBuilder(trieIn:Trie,output:String):Trie = {
      def innerTrieBuilder(trie:Trie,i:Int,cs:Array[Char]):Trie = {
        if (i == cs.length) trie << output
        else {
          val c = charMapper(cs(i))
          def buildNextLevel(dt:Trie):Trie = {
            val down = innerTrieBuilder(dt,i+1,cs)
            trie + (c -> down)
          }
          buildNextLevel(trie.nextOrElse(c,BodyTrie()))
        }
      }
      innerTrieBuilder(trieIn,0,output.toArray)
    }
    def outputBuilder(trieIn:Trie,outputsIn:OutputsMap):OutputsMap = {
      val outputsOut = outputsIn + (trieIn -> trieIn.outputs)
      trieIn.values.values.foldLeft(outputsOut)((s:OutputsMap,v:Trie) => outputBuilder(v,s))
    }
    def failureBuilder(trieIn:Trie):(FailureMap,OutputsMap) = {
      def processQueue(queue:Set[Trie],inFM:FailureMap,inOM:OutputsMap):(FailureMap,OutputsMap) = {
        if (queue.isEmpty) (inFM,inOM)
        else {
          val (r,newQueue) = (queue.head,queue.tail)
          val (incQueue:Set[Trie],incFM:FailureMap,incOM:OutputsMap) = r.values.fold((newQueue,inFM,inOM)) {
            case ((tempQueue:Set[Trie],tempFM:FailureMap,tempOM:OutputsMap),(a:Char,s:Trie)) => {
              val outQueue = tempQueue + s
              def findSuccessState(state:Trie):Trie = {
                state.next(a).fold(some = s => state,
                                   none = findSuccessState(tempFM(state)))
              }
              val endState = findSuccessState(tempFM(r))
              val down = endState.next(a).get
              val finalOM = tempOM + (s -> endState.outputs(a))
              val finalFM = tempFM + (s -> down)
              (outQueue,finalFM,finalOM)
            }
          }
          processQueue(incQueue,incFM,incOM)
        }
      }
      val initQueue:Set[Trie] = trieIn.values.values.toSet
      val initFM:FailureMap = initQueue.foldLeft(FailureMap()){ (s:FailureMap,v:Trie) => s + (v -> trieIn) }
      processQueue(initQueue,initFM,outputBuilder(trieIn,OutputsMap()))
    }
    val trie = dictionary.foldLeft(RootTrie().asInstanceOf[Trie]) { (s:Trie,v:String) => trieBuilder(s,v) }
    val (fm,om) = failureBuilder(trie)
    println("FAILURE MAP: %s\nOUTPUT MAP: %s".format(fm.toString,om.toString))
    trie
  }
}
