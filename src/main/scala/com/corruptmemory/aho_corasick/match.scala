package com.corruptmemory.aho_corasick

import AhoCorasick._
case class Match[T](start:Int,target:String,actual:String,data:T)