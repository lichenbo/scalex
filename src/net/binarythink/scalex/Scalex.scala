package net.binarythink.scalex

object Scalex {
    val helloworld = """#include "stdio.h"

int main() {
	int zero = 0;
	printf("Hell%d, w%drld!",zero,zero);
}"""
  
	def main(args:Array[String]) {
	  val loadnode = xml.XML.loadFile("C:\\CMM.xml")
	  for (token <- loadnode \ "token" if ((token \ "@skip").text != "skip")) {
//	    println(token \ "@pattern"). 
	  }
	  
	}
}