package net.binarythink.scalex

import scala.collection.mutable

object Reg2NFA {
  val memozationMap:mutable.Map[Expr, StateGraph] = mutable.Map()
  def main(args:Array[String]) {
    println(convert(RegexDef.check("(f|g|h|i|k|l|m|v|w|x|e|f)*")))
    println(convert(RegexDef.check("(g|h|i|k|l|m|f)*")))
  }
  
  def beginConvert(expr:Expr):StateGraph = {
    if (memozationMap.contains(expr)) {
      return memozationMap(expr)
    }
    val result = convert(expr)
    memozationMap.put(expr,result)
    result
  }
  
  def convert(expr:Expr):StateGraph = {
//    if (Reg2NFA.memozationMap.contains(expr)) {
//      println("Memorized " + expr + "! return " + memozationMap(expr));
//      return memozationMap(expr)
//    }
//    println("Not memorized " + expr + "!")

    expr match {
    case Literal(c: Char) => {
      val s1 = new State
      val s2 = new State
      s1.relationMap addBinding (c,s2)
      val result = new StateGraph (List(s1,s2),s1,s2)
      
      result
    }
    case Union(e1: Expr,e2: Expr) => {
      val s1 = new State
      val s2 = new State
      val g1 = convert(e1)
      val g2 = convert(e2)
      s1.relationMap addBinding('@',g1.startState)
      s1.relationMap addBinding ('@',g2.startState)
      g1.endState.relationMap addBinding ('@',s2)
      g2.endState.relationMap addBinding ('@',s2)
      val result = new StateGraph(List(s1,s2):::g1.list:::g2.list, s1, s2)
      result
    }
    case Concat(e1: Expr,e2: Expr) => {
      val s1 = new State
      val s2 = new State
      val g1 = convert(e1)
      val g2 = convert(e2)
      s1.relationMap addBinding ('@', g1.startState)
      g1.endState.relationMap addBinding ('@', g2.startState)
      g2.endState.relationMap addBinding ('@', s2)
      val result = new StateGraph(List(s1,s2):::g1.list:::g2.list, s1, s2)
      result
    }
    case Star(e:Expr) => {
      val s1 = new State
      val s2 = new State
      val s3 = new State
      val s4 = new State
      val g = convert(e)
      s1.relationMap addBinding ('@', s2)
      s2.relationMap addBinding ('@', g.startState)
      g.endState.relationMap addBinding ('@', s3)
      s3.relationMap addBinding ('@', s2)
      s3.relationMap addBinding ('@', s4)
      s1.relationMap addBinding ('@', s4)
      val result = new StateGraph(List(s1,s2,s3,s4):::g.list, s1, s4)
      result
    }
    }
  }
  class State {
  val relationMap = new mutable.HashMap[Char,mutable.Set[State]] with mutable.MultiMap[Char,State]

  // Use lazy val to avoid recomputation
  lazy val epsClosure: Set[State] = if (relationMap.contains('@')) relationMap('@').toSet | relationMap('@').toSet.flatMap((s:State) => s.epsClosure) + this else Set(this)
  def move(c:Char):Set[State] = 
    epsClosure.map((s:State) => 
      if (s.relationMap.contains(c)) 
        s.relationMap(c).map(ss => ss.epsClosure).flatten
      else Set[State]()
    ).flatten
  
}

class StateGraph(val list:List[State], val startState:State, val endState:State){ 
//  override def toString() = {
//    list.map(s => {
//      println(s)
//      s.relationMap.map(ss => {
//        println(ss._1 + " : " + ss._2)
//      })
//    })
//    "epsClosure: " + startState.epsClosure.toString + '\n' + 
//    "move: " + startState.move('a')
//  }
}


}

