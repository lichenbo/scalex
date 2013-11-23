package net.binarythink.scalex

import scala.collection.mutable.Stack

abstract class Expr
case class Star(exp:Expr) extends Expr
case class Union(exp1:Expr,exp2:Expr) extends Expr
case class Concat(exp1:Expr,exp2:Expr) extends Expr
case class Literal(chr:Char) extends Expr

object ParserExpr {
	def main(args:Array[String]) {
//	  println(RegexDef.check("abc"))
//	  println(RegexDef.check("ab|c"))
//	  println(RegexDef.check("a|b|c"))
//	  println(RegexDef.check("abc|def|g"))
//	  println(RegexDef.check("a"))
//	  println(RegexDef.check("a*"))
//	  println(RegexDef.check("ab*"))
//	  println(RegexDef.check("a*b"))
//	  println(RegexDef.check("a*b*"))
//	  println(RegexDef.check("a**"))
//	  println(RegexDef.check("a*|b"))
//	  println(RegexDef.check("a|b*"))
//	  println(RegexDef.check("a*|b*"))
//	  println(RegexDef.check("(a)"))
//	  println(RegexDef.check("(abc)"))
//	  println(RegexDef.check("(abc|def|g)"))
//	  println(RegexDef.check("(ab*)"))
//	  println(RegexDef.check("(ab)c"))
//	  println(RegexDef.check("a(bc)"))
//	  println(RegexDef.check("a|b(cd)"))
//	  println(RegexDef.check("a|(c|d)"))
//	  println(RegexDef.check("(a|b)|cd"))
//	  println(RegexDef.check("(a|b|c)d|e"))
//	  println(RegexDef.check("(ab)*"))
//	  println(RegexDef.check("(a|b*|c)*d*|e*"))
	  println(RegexDef.preprocess("a\\n"))
	  println(RegexDef.preprocess("([a-z]|[A-Z]_)*"))
	  println(RegexDef.check("a\\n"))
	} 
}

object RegexDef {
	val symbol_stack: Stack[Char] = new Stack
	val char_stack: Stack[Expr] = new Stack
	private val pri:Map[Char,Int] = Map('*'->3,'|'->2,'+'->1,'('->0)
	private var canConcat = false
	private var modEscape = false

	val token_pattern = "([a-zA-Z_\b\f\r\t\n ])".r
	private def canEvaluate (before:Char, current:Char) = pri(before)>=pri(current) 
	private def eval (c:Char) = {
			c match {
			case '+' => {
				val op2 = char_stack.pop
				val op1 = char_stack.pop
				char_stack.push(Concat(op1,op2))
			}
			case '*' => {
				val op = char_stack.pop
				char_stack.push(Star(op))
			}
			case '|' => {
				val op2 = char_stack.pop
				val op1 = char_stack.pop
				char_stack.push(Union(op1,op2))
			}
			}
	}
	def preprocess(reg:String): String = {
	  reg.replaceAllLiterally("[a-z]","(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)")
		 .replaceAllLiterally("[A-Z]","(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)")
		 .replaceAllLiterally("\\b","\b")
		 .replaceAllLiterally("\\f", "\f")
		 .replaceAllLiterally("\\r", "\r")
		 .replaceAllLiterally("\\t", "\t")
		 .replaceAllLiterally("\\n", "\n")
	}
	def check(reg:String):Expr = {
	  def pushChar(ch:Char) {
	    while(symbol_stack.length > 0 && canConcat && canEvaluate(symbol_stack.top, '+')) {
			eval(symbol_stack.pop)
		}
		if (char_stack.length > 0 && canConcat)
			symbol_stack.push('+')
		char_stack.push(Literal(ch))
		canConcat = true
		modEscape = false
	  }
	  
		for (regchar <- preprocess(reg).toList) {
			regchar.toString match {    
			case token_pattern(c) => {
				pushChar(c(0))
				// The escape situation is handled by 
			}
			case "\\" => {
			  if (modEscape) {
			    pushChar('\\')
			  } else {
			    modEscape = true
			  }
			}
			case "*" => {
			  if (modEscape)
			    pushChar('*')
			  else {  
				while(symbol_stack.length > 0 && canEvaluate(symbol_stack.top,regchar)) {
					eval(symbol_stack.pop)
				}
				symbol_stack.push('*')
				canConcat = true
			  } 
			  modEscape = false
			}
			case "(" => {
			  if (modEscape)
			    pushChar('(')
			  else {
				if (canConcat) {
				  symbol_stack.push('+')
				}
				symbol_stack.push('(')
				canConcat = false
			  }
			  modEscape = false
			}
			case ")" => {
			  if (modEscape)
			    pushChar(')')
			  else {
				while(symbol_stack.top != '(') {
					eval(symbol_stack.pop)
				}
				assert(symbol_stack.top == '(')
				symbol_stack.pop
				canConcat = true
			  }
			  modEscape = false
			}
			case "|" => {
			  if (modEscape)
			    pushChar('|')
			  else {
				while(symbol_stack.length > 0 && canEvaluate(symbol_stack.top,regchar)) {
					eval(symbol_stack.pop)
				}
				symbol_stack.push('|')
				canConcat = false
			  }
			  modEscape = false
			}
			}
		}
		
		/* Assure the last calc */
		while (symbol_stack.length > 0) 
		  eval(symbol_stack.pop)
		
		assert(symbol_stack.length == 0)
		assert(char_stack.length == 1)
		canConcat = false
		modEscape = false
		return char_stack.pop
	}
}