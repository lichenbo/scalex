package net.binarythink.scalex

object MatchFailedException extends Exception

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
//	  printMatch("a[a-z]*","babc")
//	  printMatch("af*","aff")
//	  printMatch("[a-z]*","aff") //Matched
	  println(matchLeft("a[a-z]*","aabc_dsf"))
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
	
	def matchLeft(sg:NFA2DFA.StateGraph, str:String):String = {
	  var curState = sg.startState
	  val strSeq = str.toList

	  for (pos <- 0 to strSeq.size-1) {
	    val c = strSeq(pos)
	    if (curState.relationMap.keySet.contains(c)) {
	      curState = curState.move(c)
	    }
	    else if (sg.endState.contains(curState)) {
	      return strSeq.drop(pos).mkString
	    }
	    else {
	      throw MatchFailedException
	    }
	  }
	  if (sg.endState.contains(curState)) {
	    return ""
	  } else {
	    throw MatchFailedException
	  }
	}
	
	def rmatch(reg:String, str:String): Boolean = {
		rmatch(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check(reg))),str)
	}
	
	def matchLeft(reg:String, str:String): String= {
	  try {
	    matchLeft(NFA2DFA.convert(Reg2NFA.convert(RegexDef.check(reg))),str)
	  } catch {
	    case MatchFailedException => {
	      println("Match failed")
	      ""
	    }
	  }
	}
	
	def printMatch(reg:String, str:String) = {
	  if (rmatch(reg,str)) println(reg + " : " + str + " Matched")
	  else println(reg + " : " + str + " NOT MATCHED!")
	}
}