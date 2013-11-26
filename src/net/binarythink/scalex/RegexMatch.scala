package net.binarythink.scalex

object RegexMatch {
	def main(args:Array[String]) {
//	  printMatch("a|bc","ac")
//	  printMatch("a|bc","a")
//	  printMatch("a|(bc)","a")
//	  printMatch("a|(bc)","bc")
//	  printMatch("a|(bc)", "abc")
//	  printMatch("a|b|c", "ab")
//	  printMatch("a|bc*","ac")
//	  printMatch("a|bc*","aac")
//	  printMatch("a|bc*","acccccccccc")
//	  printMatch("(a|b)*c","abbababababc")
//	  printMatch("\\na","\na")
//	  printMatch("s[a-z]*","sdfsaf")
//	  printMatch("_(a|b)*","_aabaa")
//	  printMatch("a[a-z]*","aff")
//	  printMatch("af*","aff")
//	  printMatch("[a-z]*","aff") //Matched
	}
	
	def rmatch(sg:NFA2DFA.StateGraph, str:String): Boolean = {
	  var curState = sg.startState
//	  println(sg)
	  for (c <- str.toList) {
	    if (curState.relationMap.keySet.contains(c))
		    curState = curState.move(c)
		else return false
	  }
	  sg.endState.contains(curState)
	}
	
	def matchLeft(sg:NFA2DFA.StateGraph, str:String):String = {
	  var curState = sg.startState
	  var strStream = str.toStream
	  var c:Char = '0'
	  while (c = strStream.readChar) {
	    if (curState.relationMap.ketSet.contains(c))
	      curState=curState.move(c)
	    else if (sg.endState.contains(curState)) {
	      return strStream.toString
	    }
	  }
	  if (sg.endState.contains(curState)) {}
	}
	
	def rmatch(reg:String, str:String): Boolean = {
		rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check(reg))),str)
	}
	
	def printMatch(reg:String, str:String) = {
	  if (rmatch(reg,str)) println(reg + " : " + str + " Matched")
	  else println(reg + " : " + str + " NOT MATCHED!")
	}
}