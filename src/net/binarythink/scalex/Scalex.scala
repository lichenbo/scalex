package net.binarythink.scalex

import java.io._;

object Scalex {
    val helloworld:String = """
#include "stdio.h"

int main() {
	int zero = 0;
	printf("Hell%d, w%drld!",zero,zero);
}"""
    val loadnode = xml.XML.loadFile("CMM.xml")
    val regexAll:String = ((for (token <- loadnode\"token") yield (token\"@pattern").text)).mkString("(",")|(",")")
    val tokenList:List[(String,String)] = (for (token <- loadnode\"token") yield ((token\"@pattern").text,(token\"@name").text)).toList
    var writer:PrintWriter = null;

    def main(args:Array[String]) {
      writer = new PrintWriter("helloworld.token")
	  analyse(helloworld)
	  writer.close
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
	    val token = convert2Token(currentMatch)
	    println(token)
	    writer.println(token)
	    println("Match finished.")
	  } else {
	    val token = convert2Token(currentMatch)
	    println(token)
	    writer.println(token)
	    analyse(nextMatch)
	  }
      
    }
}