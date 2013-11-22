import scala.collection.mutable
object NFA2DFA {
  def main(args:Array[String]) {
    convert(Reg2NFA.convert(RegexDef.check("a|bc")))
  }
  
  def convert(nfa:Reg2NFA.StateGraph):StateGraph = {
    val symbolTable:Set[Char] = nfa.list.foldLeft(Set[Char]())((ss:Set[Char],s:Reg2NFA.State) => ss | s.relationMap.keys.toSet) -- Set('@')
    assert(symbolTable.contains('@') == false)
    val startState = new State(nfa.startState.epsClosure)
  	buildState(startState)
  	val sg = new StateGraph(startState)
  	sg.addState(startState)
  	for (s <- sg.set) {
  	  if (s.built == false) {
  	    buildState(s)
  	  }
  	}
    
    def buildState(s:State) = {
      for (symbol:Char <- symbolTable) {
        val nfaList = for (nfaState:Reg2NFA.State <- s.nfaStateList) 
         yield nfaState.move(symbol)
        val newState = sg.getState(nfaList.flatten)
        if (nfaList.flatten.contains(nfa.endState)) {
          sg.endState.add(newState)
        }
        s.relationMap(symbol) = newState 
      }
      s.built = true
    }
    
    return sg 
  }
  
  class StateGraph (startState:State){
    val set:mutable.Set[State] = mutable.Set[State]()
    val endState:mutable.Set[State] = mutable.Set[State]()

    def addState(s:State) {
      set.add(s)
    }

    def getState(nfaStateList:Set[Reg2NFA.State]):State = {
      assert(set.filter(s => s.nfaStateList == nfaStateList).size < 2)  // Confirm the filter result is at most 1. Otherwise the Set mechanics is buggy
      set.filter(s => s.nfaStateList == nfaStateList).headOption match {
        case Some(s) => s
        case None => {
          val ss = new State(nfaStateList)
          addState(ss)
          ss
        }
      }
    }

  }
  
  class State (val nfaStateList:Set[Reg2NFA.State]){
    var built:Boolean = false
    val relationMap:mutable.HashMap[Char,State] = mutable.HashMap[Char,State]()]
  }