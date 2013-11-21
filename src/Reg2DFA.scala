

object Reg2eDFA {
  def main(args:Array[String]) {
    
  }
  
  def convert(expr:Expr) = expr match {
    case Literal(c) => {
      val s1 = new State
      val s2 = new State
      s1.relationMap = Map[Char,List[State]]()
      val graph = new StateGraph (List(new S))

    }
    case Union(u1,u2) => ???
    case Concat(c1,c2) => ??? 
    case Star(s) => ???
  }
  
  def combine(sg1:StateGraph, sg2:StateGraph):StateGraph = ???
}

class State (){
  val relationMap:Map[Char,List[State]] = Map[Char,List[State]]()
  
}

class StateGraph(list:List[State],startState:State,endState:State){ 
  
}

