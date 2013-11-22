import scala.collection.mutable

object Reg2NFA {
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
      val g1 = convert(e1)
      val g2 = convert(e2)
      s1.relationMap addBinding('@',g1.startState)
      s1.relationMap addBinding ('@',g2.startState)
      g1.endState.relationMap addBinding ('@',s2)
      g2.endState.relationMap addBinding ('@',s2)
      new StateGraph(List(s1,s2):::g1.list:::g2.list, s1, s2)
    }
    case Concat(e1,e2) => {
      val s1 = new State
      val s2 = new State
      val g1 = convert(e1)
      val g2 = convert(e2)
      s1.relationMap addBinding ('@', g1.startState)
      g1.endState.relationMap addBinding ('@', g2.startState)
      g2.endState.relationMap addBinding ('@', s2)
      new StateGraph(List(s1,s2):::g1.list:::g2.list, s1, s2)
    }
    case Star(e) => {
      val s1 = new State
      val s2 = new State
      val s3 = new State
      val s4 = new State
      val g = convert(e)
      s1.relationMap addBinding ('@', s2)
      s2.relationMap addBinding ('@', g.startState)
      g.endState.relationMap addBinding ('@', s3)
      s3.relationMap addBinding ('@', s2)
      s3.relationMap addBinding ('@', s4)
      new StateGraph(List(s1,s2,s3,s4):::g.list, s1, s4)
    }
  }
  class State {
  val relationMap = new mutable.HashMap[Char,mutable.Set[State]] with mutable.MultiMap[Char,State]

  def epsClosure: Set[State] = if (relationMap.contains('@')) relationMap('@').toSet | relationMap('@').toSet.flatMap((s:State) => s.epsClosure) + this else Set()
  def move(c:Char):Set[State] = 
    epsClosure.map((s:State) => 
      if (s.relationMap.contains(c)) 
        s.relationMap(c).map(ss => ss.epsClosure).flatten
      else Set()
    ).flatten
  
}

class StateGraph(val list:List[State], val startState:State, val endState:State){ 
  override def toString() = {
    list.map(s => {
      println(s)
      s.relationMap.map(ss => {
        println(ss._1 + " : " + ss._2)
      })
    })
    "epsClosure: " + startState.epsClosure.toString + '\n' + 
    "move: " + startState.move('a')
  }
}


}

