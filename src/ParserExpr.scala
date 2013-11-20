import scala.collection.mutable.Stack
import scala.util.matching.Regex

abstract class Expr
case class Star(exp:Expr) extends Expr
case class Union(exp1:Expr,exp2:Expr) extends Expr
case class Concat(exp1:Expr,exp2:Expr) extends Expr
case class Literal(chr:Char) extends Expr

object ParserExpr {
	def main(args:Array[String]) {
	  println(RegexDef.check("abc"))
	  println(RegexDef.check("ab|c"))
	  println(RegexDef.check("a|b|c"))
	  println(RegexDef.check("abc|def|g"))
	  println(RegexDef.check("a"))
	  println(RegexDef.check("a*"))
	  println(RegexDef.check("ab*"))
	  println(RegexDef.check("a*b"))
	  println(RegexDef.check("a*b*"))
	  println(RegexDef.check("a**"))
	  println(RegexDef.check("a*|b"))
	  println(RegexDef.check("a|b*"))
	  println(RegexDef.check("a*|b*"))
	  println(RegexDef.check("(a)"))
	  println(RegexDef.check("(abc)"))
	  println(RegexDef.check("(abc|def|g)"))
	  println(RegexDef.check("(ab*)"))
	  println(RegexDef.check("(ab)c"))
	  println(RegexDef.check("a(bc)"))
	  println(RegexDef.check("a|b(cd)"))
	  println(RegexDef.check("a|(c|d)"))
	  println(RegexDef.check("(a|b)|cd"))
	  println(RegexDef.check("(a|b|c)d|e"))
	  println(RegexDef.check("(ab)*"))
	  println(RegexDef.check("(a|b*|c)*d*|e*"))
	} 
}

object RegexDef {
	val symbol_stack: Stack[Char] = new Stack
	val char_stack: Stack[Expr] = new Stack
	private val pri:Map[Char,Int] = Map('*'->3,'|'->2,'+'->1,'('->0)
	private var canConcat = false

	val token_pattern = "([a-zA-Z_])".r
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
			println("CharStack:" + char_stack)
	}

	def check(reg:String):Expr = {
		for (regchar <- reg.toList) {
			regchar.toString match {    
			case token_pattern(c) => {
				while(symbol_stack.length > 0 && canConcat && canEvaluate(symbol_stack.top, '+')) {
					eval(symbol_stack.pop)
				}
				if (char_stack.length > 0 && canConcat)
					symbol_stack.push('+')
				char_stack.push(Literal(c(0)))
				canConcat = true
			}
			case "*" => {
				while(symbol_stack.length > 0 && canEvaluate(symbol_stack.top,regchar)) {
					eval(symbol_stack.pop)
				}
				symbol_stack.push('*')
				canConcat = true
			}
			case "(" => {
				if (canConcat) {
				  symbol_stack.push('+')
				}
				symbol_stack.push('(')
				canConcat = false
			}
			case ")" => {
				while(symbol_stack.top != '(') {
					eval(symbol_stack.pop)
				}
				assert(symbol_stack.top == '(')
				symbol_stack.pop
				canConcat = true
			}
			case "|" => {
				while(symbol_stack.length > 0 && canEvaluate(symbol_stack.top,regchar)) {
					eval(symbol_stack.pop)
				}
				symbol_stack.push('|')
				canConcat = false
			}
			}
			println("Symbol_stack:" + symbol_stack)
		}
		
		/* Assure the last calc */
		while (symbol_stack.length > 0) 
		  eval(symbol_stack.pop)
		
		println("Final symbol:" + symbol_stack)
		println("Final char:" + char_stack)
		assert(symbol_stack.length == 0)
		assert(char_stack.length == 1)
		canConcat = false
		return char_stack.pop
	}
}