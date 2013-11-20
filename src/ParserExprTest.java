import static org.junit.Assert.*;
import org.junit.Test;


public class ParserExprTest {

	@Test
	public void testCheck1() {
		assertEquals("abc",RegexDef.check("abc"),new Concat(new Concat(new Literal('a'),new Literal('b')),new Literal('c')));
	}
	@Test
	public void testCheck2() {
		assertEquals("ab|c",RegexDef.check("ab|c"),new Union(new Concat(new Literal('a'), new Literal('b')), new Literal('c')));
	}

	@Test
	public void testCheck3() {
		assertEquals("a|b|c",RegexDef.check("a|b|c"),new Union(new Union(new Literal('a'), new Literal('b')), new Literal('c')));
	}
	@Test
	public void testCheck4() {
		assertEquals("abc",RegexDef.check("abc"),new Concat(new Concat(new Literal('a'),new Literal('b')),new Literal('c')));
	}
	@Test
	public void testCheck5() {
		assertEquals("abc",RegexDef.check("abc"),new Concat(new Concat(new Literal('a'),new Literal('b')),new Literal('c')));
	}
}
