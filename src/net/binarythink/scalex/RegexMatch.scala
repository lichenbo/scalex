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
//	  println(matchLeft("a[a-z]*","aabc_dsf"))
//	  printMatch("\"([a-zA-Z0-9_]*)\"","\"a89\"")
//	  println(matchLeft("(\\n)|(<=)|(>=)|(( |\\b|\\f|\\r|\\t)( |\\b|\\f|\\r|\\t)*)|(<|>|#|.|\\(|\\)|;|,|+|-|\\*|/|=|{|})|([0-9][0-9]*)|(\"([a-zA-Z0-9_]|[Chars])*\")|(int)|([a-zA-Z_][a-zA-Z0-9_]*)","include \"stdio.h\""))
//	  println(matchLeft("(\"([a-zA-Z0-9_]| )*\")|([a-zA-Z_][a-zA-Z0-9_]*)","include \"stdioh\""))
//	  println(matchLeft("(\"([a-zA-Z0-9_]| )*\")","include \"stdioh\""))
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
	
	def matchLeft(sg:NFA2DFA.StateGraph, str:String):(String,String) = {
	  var curState = sg.startState
	  val strSeq = str.toList

	  for (pos <- 0 to strSeq.size-1) {
	    val c = strSeq(pos)
	    if (curState.relationMap.keySet.contains(c)) {
	      curState = curState.move(c)
	    }
	    else if (sg.endState.contains(curState)) {
	      return (strSeq.take(pos).mkString,strSeq.drop(pos).mkString)
	    }
	    else {
	      throw MatchFailedException
	    }
	  }
	  if (sg.endState.contains(curState)) {
	    return (strSeq.mkString,"")
	  } else {
	    throw MatchFailedException
	  }
	}
	
	def rmatch(reg:String, str:String): Boolean = {
	    println("Matching " + reg)
		rmatch(NFA2DFA.convert(Reg2NFA.beginConvert(RegexDef.check(reg))),str)
	}
	
	def matchLeft(reg:String, str:String): (String,String)= {
	  try {
	    println("Matching " + reg)
	    matchLeft(NFA2DFA.convert(Reg2NFA.beginConvert(RegexDef.check(reg))),str)
	  } catch {
	    case MatchFailedException => {
	      ("","")	// use (emptyString,emptyString) to mark an error, since an emptyString can never be matched
	    }
	  }
	}
	
	def printMatch(reg:String, str:String) = {
	  if (rmatch(reg,str)) println(reg + " : " + str + " Matched")
	  else println(reg + " : " + str + " NOT MATCHED!")
	}
}