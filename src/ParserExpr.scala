import scala.collection.mutable.Stack

class Expr
case class Star(exp:Expr) extends Expr
case class Union(exp:Expr) extends Expr
case class Concat(exp:Expr) extends Expr
case class Literal(chr:Char) extends Expr

object ParserExpr {
  def main(args:Array[String]) {
    
    
  }
  
}

object RegexDef {
  val symbol_stack: Stack[Char] = new Stack
  val char_stack: Stack[Expr] = new Stack
  
  val token_pattern = "[a-zA-Z_]".r
  def check(reg:String) = for (regchar <- reg.toList)
    regchar match {
    case token_pattern(c) => {char_stack.push(Literal(c(0))); symbol_stack.push('+')}
    case '*' => symbol_stack.push('*')
    case '(' => symbol_stack.push('(')
    case ')' =>
    case '|' => symbol_stack.push('|')
    }
}