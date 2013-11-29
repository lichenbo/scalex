package net.binarythink.scalex

object Scalex {
    var helloworld:String = """#include "stdio.h"

int main() {
	int zero = 0;
	printf("Hell%d, w%drld!",zero,zero);
}"""
    val testString:String = "#include dfdfd"
    val loadnode = xml.XML.loadFile("C:\\CMM.xml")
    val regexAll:String = (for (token <- loadnode\"token") yield (token\"@pattern").text).mkString("(",")|(",")")
    val tokenList:List[(String,String)] = (for (token <- loadnode\"token") yield ((token\"@pattern").text,(token\"@name").text)).toList

    def main(args:Array[String]) {
	  analyse(testString)
	//helloworld = RegexMatch.matchLeft((token \ "pattern").toString, helloworld)
	}
    
    def convert2Token(str:String):String = {
      for (tokenPair <- tokenList) {
        if (RegexMatch.rmatch(tokenPair._1,str)) {
          return tokenPair._2
        }
      }
      ""
    }
    
    def analyse(nextString:String) {
      val (currentMatch,nextMatch) = RegexMatch.matchLeft(regexAll, nextString)
	  if (currentMatch == "") {
		println("Cannot be matched.")
	  } else if (nextMatch == "") {
	    println("Match finished.")
	  } else {
	    println(convert2Token(currentMatch))
	    RegexMatch.matchLeft(regexAll, nextMatch)
	    analyse(nextMatch)
	  }
      
    }
}