package net.binarythink.scalex;
import static org.junit.Assert.*;
import org.junit.Test;


public class ParserExprTest {

	@Test
	public void testRexDef1() {
		assertEquals("abc",RegexDef.check("abc"),new Concat(new Concat(new Literal('a'),new Literal('b')),new Literal('c')));
	}
	@Test
	public void testRexDef2() {
		assertEquals("ab|c",RegexDef.check("ab|c"),new Concat(new Literal('a'), new Union(new Literal('b'), new Literal('c'))));
	}

	@Test
	public void testRexDef3() {
		assertEquals("a|b|c",RegexDef.check("a|b|c"),new Union(new Union(new Literal('a'), new Literal('b')), new Literal('c')));
	}
	@Test
	public void testRexDef4() {
		assertEquals("abc",RegexDef.check("abc"),new Concat(new Concat(new Literal('a'),new Literal('b')),new Literal('c')));
	}
	@Test
	public void testReg2NFA1() {
		Reg2NFA.State s1 = new Reg2NFA.State();
		Reg2NFA.State s2 = new Reg2NFA.State();
		Reg2NFA.State s3 = new Reg2NFA.State();
		Reg2NFA.State s4 = new Reg2NFA.State();
		Reg2NFA.State s5 = new Reg2NFA.State();
		Reg2NFA.State s6 = new Reg2NFA.State();
		Reg2NFA.State s7 = new Reg2NFA.State();
		Reg2NFA.State s8 = new Reg2NFA.State();
		Reg2NFA.State s9 = new Reg2NFA.State();
		

	}
	@Test
	public void testRex2NFA2() {
		assertEquals("abc",RegexDef.check("abc"),new Concat(new Concat(new Literal('a'),new Literal('b')),new Literal('c')));
	}
}
