object RegexMatch {
	def main(args:Array[String]) {
	  NFA2DFA.convert(Reg2NFA.convert(RegexDef.check("a|bc")))
	}
	
	def rmatch(sg:NFA2DFA.StateGraph, str:String): Boolean = {
	  for (c <- str.toList) {
	    
	  }
	  true
	}
}