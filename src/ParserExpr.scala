import scala.collection.mutable.Stack
import scala.util.matching.Regex

abstract class Expr
case class Star(exp:Expr) extends Expr
case class Union(exp1:Expr,exp2:Expr) extends Expr
case class Concat(exp1:Expr,exp2:Expr) extends Expr
case class Literal(chr:Char) extends Expr

object ParserExpr {
	def main(args:Array[String]) {
		RegexDef.check("abc")
	} 
}

object RegexDef {
	val symbol_stack: Stack[Char] = new Stack
	val char_stack: Stack[Expr] = new Stack
	private val pri:Map[Char,Int] = Map('*'->3,'|'->2,'+'->1)

	val token_pattern = "([a-zA-Z_])".r
	private def canEvaluate (before:Char, current:Char) = pri(before)>=pri(current) 
	private def eval (c:Char) = c match {
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
	}; println("CharStack:" + char_stack)

	def check(reg:String) {
		for (regchar <- reg.toList) {
			regchar.toString match {    
			case token_pattern(c) => {
				while(symbol_stack.length > 0 && char_stack.length > 1 &&  symbol_stack.top != '|' && canEvaluate(symbol_stack.top, '+')) {
					eval(symbol_stack.pop)
				}
				char_stack.push(Literal(c(0)))
				symbol_stack.push('+')
			}
			case "*" => {
				while(symbol_stack.length > 0 && canEvaluate(symbol_stack.top,regchar)) {
					eval(symbol_stack.pop)
				}
				symbol_stack.push('*')
			}
			case "(" => {
				while(symbol_stack.length > 0 && canEvaluate(symbol_stack.top,regchar)) {
					eval(symbol_stack.pop)
				}
				symbol_stack.push('(')
			}
			case ")" => {
				while(symbol_stack.top != '(') {
					eval(symbol_stack.pop)
				}
				assert(symbol_stack.top == '(')
				symbol_stack.pop
			}
			case "|" => {
				while(symbol_stack.length > 0 && canEvaluate(symbol_stack.top,regchar)) {
					eval(symbol_stack.pop)
				}
				symbol_stack.push('|')
			}
			}
			println("Symbol_stack:" + symbol_stack)
		}
		println(char_stack)
		println(symbol_stack)
	}
}