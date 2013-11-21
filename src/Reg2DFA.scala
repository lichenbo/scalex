import scala.collection.mutable

object Reg2eDFA {
  def main(args:Array[String]) {
    println(convert(RegexDef.check("a|bc")))
  }
  
  def convert(expr:Expr):StateGraph = expr match {
    case Literal(c: Char) => {
      val s1 = new State
      val s2 = new State
      s1.relationMap addBinding (c,s2)
      new StateGraph (List(s1,s2),s1,s2)
    }
    case Union(e1: Expr,e2: Expr) => {
      val s1 = new State
      val s2 = new State
      s1.relationMap addBinding('@',convert(e1).startState)
      s1.relationMap addBinding ('@',convert(e2).startState)
      convert(e1).endState.relationMap addBinding ('@',s2)
      convert(e2).endState.relationMap addBinding ('@',s2)
      new StateGraph(List(s1,s2):::convert(e1).list:::convert(e2).list, s1, s2)
    }
    case Concat(e1,e2) => {
      val s1 = new State
      val s2 = new State
      s1.relationMap addBinding ('@', convert(e1).startState)
      convert(e2).endState.relationMap addBinding ('@', s2)
      new StateGraph(List(s1,s2):::convert(e1).list:::convert(e2).list, s1, s2)
    }
    case Star(e) => {
      val s1 = new State
      val s2 = new State
      val s3 = new State
      val s4 = new State
      s1.relationMap addBinding ('@', s2)
      s2.relationMap addBinding ('@', convert(e).startState)
      convert(e).endState.relationMap addBinding ('@', s3)
      s3.relationMap addBinding ('@', s2)
      s3.relationMap addBinding ('@', s4)
      new StateGraph(List(s1,s2,s3,s4):::convert(e).list, s1, s4)
    }
  }
  
}

class State {
  val relationMap = new mutable.HashMap[Char,mutable.Set[State]] with mutable.MultiMap[Char,State]
  override def toString() = {
    relationMap.mkString
  }
}

class StateGraph(val list:List[State], val startState:State, val endState:State){ 
  override def toString() = {
    startState.toString
  }
}

