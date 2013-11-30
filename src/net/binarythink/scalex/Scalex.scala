package net.binarythink.scalex

object Scalex {
    val helloworld:String = """
#include "stdio.h"

int main() {
	int zero = 0;
	printf("Hell%d, w%drld!",zero,zero);
}"""
    val loadnode = xml.XML.loadFile("C:\\CMM.xml")
    val regexAll:String = ((for (token <- loadnode\"token") yield (token\"@pattern").text)).mkString("(",")|(",")")
    val tokenList:List[(String,String)] = (for (token <- loadnode\"token") yield ((token\"@pattern").text,(token\"@name").text)).toList

    def main(args:Array[String]) {
	  analyse(helloworld)
	}
    
    def convert2Token(str:String):String = {
      println("Converting " + str + "to token")
      for (tokenPair <- tokenList) {
        if (RegexMatch.rmatch(tokenPair._1,str)) {
          return tokenPair._2
        }
      }
      println(str)
      assert(false)
      ""
    }
    
    def analyse(nextString:String) {
      val (currentMatch,nextMatch) = RegexMatch.matchLeft(regexAll, nextString)
	  if (currentMatch == "") {
		println("Cannot be matched.")
	  } else if (nextMatch == "") {
	    println(convert2Token(currentMatch))
	    println("Match finished.")
	  } else {
	    println(convert2Token(currentMatch))
	    analyse(nextMatch)
	  }
      
    }
}