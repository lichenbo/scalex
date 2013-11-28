package net.binarythink.scalex

object Scalex {
    var helloworld:String = """#include "stdio.h"

int main() {
	int zero = 0;
	printf("Hell%d, w%drld!",zero,zero);
}"""
  
	def main(args:Array[String]) {
	  val loadnode = xml.XML.loadFile("C:\\CMM.xml")
	  for (token <- loadnode \ "token") {
	    helloworld = RegexMatch.matchLeft((token \ "pattern").toString, helloworld)
	  }
	  
	}
}