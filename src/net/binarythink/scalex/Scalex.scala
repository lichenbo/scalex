package net.binarythink.scalex

import java.io._;

object Scalex {
    var loadnode = xml.XML.loadFile("CMM.xml")
    var regexAll:String = ((for (token <- loadnode\"token") yield (token\"@pattern").text)).mkString("(",")|(",")")
    var tokenList:List[(String,String)] = (for (token <- loadnode\"token") yield ((token\"@pattern").text,(token\"@name").text)).toList
    var skipList:List[String] = null
   
    var reader:io.BufferedSource = null
    var writer:PrintWriter = null

    def main(args:Array[String]) {
      loadnode = xml.XML.loadFile(args(0))
      regexAll = ((for (token <- loadnode\"token") yield (token\"@pattern").text)).mkString("(",")|(",")")
      tokenList = (for (token <- loadnode\"token") yield ((token\"@pattern").text,(token\"@name").text)).toList
      skipList = (for (token <- loadnode\"token" if token.attributes.contains("skip")) yield )
      reader = scala.io.Source.fromFile(args(1))
      writer = new PrintWriter(args(2))
      val content = reader.mkString
	  analyse(content)
	  writer.close
	  reader.close
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