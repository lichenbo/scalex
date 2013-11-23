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
		

	}
	@Test
	public void testRex2NFA2() {
		assertEquals("abc",RegexDef.check("abc"),new Concat(new Concat(new Literal('a'),new Literal('b')),new Literal('c')));
	}
}
