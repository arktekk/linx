package linx

import org.junit.Test
import org.junit.Assert._

class LinxTest {
  @Test
  def empty{
    val path = "/"
    assertEquals(Root(), path)
    val Root() = path
  }

  @Test
  def variable{
    val X = Root / *
    val path = "/x"
    val X(x) = path
    assertEquals("x", x)
    assertEquals(X(x), path)
  }

  def variableMulti{
    val X = Root / * / * / *
    val path = "/a/b/c"
    val X(a,b,c) = path
    assertEquals("a", a)
    assertEquals("b", b)
    assertEquals("c", c)
    assertEquals(path, X(a,b,c))
  }

  @Test
  def literal{
    val X = Root / "A" / "B" / "C"
    val path = "/A/B/C"
    assertEquals(path, X())
    val X() = path
  }

  @Test
  def mixed{
    val X = Root / "A" / * / "C" / "D" / * / * / "G" / * / "I"
    val path = "/A/B/C/D/E/F/G/H/I"
    val X(b, e, f, h) = path
    assertEquals("B", b)
    assertEquals("E", e)
    assertEquals("F", f)
    assertEquals("H", h)
    assertEquals(path, X(b, e, f, h))
  }

  @Test
  def failRoot{
    assertFalse(Root.unapply("/a"))
  }

  @Test
  def failLiteral{
    val A = Root / "A"
    assertFalse(A.unapply("/"))
    assertFalse(A.unapply("/B"))
    assertFalse(A.unapply("/A/B"))
  }

  @Test
  def failVariable{
    val A = Root / *
    assertEquals(None, A.unapply("/"))
    assertEquals(None, A.unapply("/A/B"))
  }

  @Test
  def failMixed{
    val A = Root / * / "A"
    assertEquals(None, A.unapply("/"))
    assertEquals(None, A.unapply("/A/B"))
    assertEquals(None, A.unapply("/B/A/C"))
  }
}
