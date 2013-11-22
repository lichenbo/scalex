object NFA2DFA {
  def main(args:Array[String]) {
    convert(Reg2NFA.convert(RegexDef.check("a|bc")))
  }
  
  def convert(nfa:StateGraph) {
    
  }
}