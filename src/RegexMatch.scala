object RegexMatch {
	def main(args:Array[String]) {
	  rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check("a|bc"))),"ac")
	  rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check("a|(bc)"))),"a")
	  rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check("a|(bc)"))),"bc")
	  rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check("a|bc*"))),"ac")
	  rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check("a|bc*"))),"acccccccccc")
	  rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check("(a|b)*c"))),"abbababababc")
	}
	
	def rmatch(sg:NFA2DFA.StateGraph, str:String): Boolean = {
	  var curState = sg.startState
	  for (c <- str.toList) {
	    curState = curState.move(c)
	  }
	  true
	}
}