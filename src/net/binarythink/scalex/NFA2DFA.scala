package net.binarythink.scalex

import scala.collection.mutable
object NFA2DFA {
  val memozationMap:mutable.Map[Reg2NFA.StateGraph, NFA2DFA.StateGraph] = mutable.Map()
  def main(args:Array[String]) {
    println(convert(Reg2NFA.convert(RegexDef.check("ac*"))))
  }
  
  def convert(nfa:Reg2NFA.StateGraph):StateGraph = {
    println("begin NFA to DFA")
    if (memozationMap.contains(nfa)) {
      println("Memorized dfa!")
      return memozationMap(nfa)
    }
    println("Do not remember dfa!")
    val symbolTable:Set[Char] = nfa.list.foldLeft(Set[Char]())((ss:Set[Char],s:Reg2NFA.State) => ss | s.relationMap.keys.toSet) -- Set('@') // collect all symbols meaningful
    assert(symbolTable.contains('@') == false)
    val startState = new State(nfa.startState.epsClosure)
  	val sg = new StateGraph(startState)
  	buildState(startState)
  	sg.addState(startState)
  	while (!sg.set.forall(s => s.built)) {
  	  for (s <- sg.set)
  	  if (!s.built) {
  	    buildState(s)
  	  }
  	}
    assert(sg.set.forall(s => s.built))
    
    def buildState(s:State) = {
      for (symbol:Char <- symbolTable) {
        val nfaList = for (nfaState:Reg2NFA.State <- s.nfaStateList) 
         yield nfaState.move(symbol)
//        println(s.nfaStateList)
//        println(symbol)
//        println(nfaList)
        if (nfaList.flatten != Set()) {
	        val newState = sg.getState(nfaList.flatten)
	        if (nfaList.flatten.contains(nfa.endState)) {
	          sg.endState.add(newState)
	        }
	        s.relationMap(symbol) = newState 
        }
      }
      s.built = true
    }
    
    memozationMap.put(nfa, sg)
    return sg 
  }
  
  class StateGraph (val startState:State){
    val set:mutable.Set[State] = mutable.Set[State]()
    val endState:mutable.Set[State] = mutable.Set[State]()

    def addState(s:State) {
      set.add(s)
    }

    def getState(nfaStateList:Set[Reg2NFA.State]):State = {
      assert(set.filter(s => s.nfaStateList == nfaStateList).size < 2)  // Confirm the filter result is at most 1. Otherwise the Set mechanics is buggy
      assert(nfaStateList.size > 0)
      set.filter(s => s.nfaStateList == nfaStateList).headOption match {
        case Some(s) => s
        case None => {
          val ss = new State(nfaStateList)
          addState(ss)
          ss
        }
      }
    }
    override def toString() = {
      for (m <- set) {
        println(m)
        m.relationMap.map(ss => {
          println(ss._1 + ":" + ss._2)
        })
      }
      "Set: " + set.toString + '\n' +
      "startState: " + startState + '\n' + 
      "endState: " + endState
    }

  }
  
  class State (val nfaStateList:Set[Reg2NFA.State]){
    var built:Boolean = false
    val relationMap:mutable.HashMap[Char,State] = mutable.HashMap[Char,State]()
    def move(c:Char):State = relationMap(c)
    override def toString () = {
      hashCode.toString + '\n' + "nfaStates:" + nfaStateList + '\n'
    }
//    override def toString() = {
//      for (m <- relationMap) {
//	      println("state: " + hashCode.toString + ": " + m._1 + "->" + m._2)
//      }
//      ""
//    }
  }
}