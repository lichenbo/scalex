import scala.collection.mutable.Stack

class Expr
case class Star(exp:Expr) extends Expr
case class Union(exp1:Expr,exp2:Expr) extends Expr
case class Concat(exp1:Expr,exp2:Expr) extends Expr
case class Literal(chr:Char) extends Expr

object ParserExpr {
  def main(args:Array[String]) {
    
    
  }
  
}

object RegexDef {
  val symbol_stack: Stack[Char] = new Stack
  val char_stack: Stack[Expr] = new Stack
  private val pri = Map('*'->3,'|'->2,'+'->1)
  
  val token_pattern = "[a-zA-Z_]".r
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
  }
  
  def check(reg:String) = 
    for (regchar <- reg.toList) regchar match {
    case token_pattern(c) => {
      char_stack.push(Literal(c(0)))
      if (symbol_stack.top != '|') {
        symbol_stack.push('+')
      }
    }
    case '*' => 
      if(canEvaluate(symbol_stack.top,regchar)) {
        val op = char_stack.pop
        char_stack.push(Star(op))
      } else {
        symbol_stack.push('*')
      }
    case '(' => symbol_stack.push('(')
    case ')' =>
    case '|' => 
      if(canEvaluate(symbol_stack.top,regchar)) {
        val op2 = char_stack.pop
        val op1 = char_stack.pop
        char_stack.push(Union(op1,op2))
      } else {
        symbol_stack.push('|')
      }
    }
}