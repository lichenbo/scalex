package net.binarythink.scalex

object RegexMatch {
	def main(args:Array[String]) {
	  printMatch("a|bc","ac")
	  printMatch("a|bc","a")
	  printMatch("a|(bc)","a")
	  printMatch("a|(bc)","bc")
	  printMatch("a|bc*","ac")
	  printMatch("a|bc*","aac")
	  printMatch("a|bc*","acccccccccc")
	  printMatch("(a|b)*c","abbababababc")
	}
	
	def rmatch(sg:NFA2DFA.StateGraph, str:String): Boolean = {
	  var curState = sg.startState
	  for (c <- str.toList) {
	    if (curState.relationMap.keySet.contains(c))
		    curState = curState.move(c)
		else return false
	  }
	  sg.endState.contains(curState)
	}
	
	def rmatch(reg:String, str:String): Boolean = {
		rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check(reg))),str)
	}
	
	def printMatch(reg:String, str:String) = {
	  if (rmatch(reg,str)) println(reg + " : " + str + " Matched")
	  else println(reg + " : " + str + " NOT MATCHED!")
	}
}