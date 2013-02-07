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
    val X = Root / 'x
    val path = "/x"
    val X(x) = path
    assertEquals("x", x)
    assertEquals(X(x), path)
  }

  def variableMulti{
    val X = Root / 'a / 'b / 'c
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
    val X = Root / "A" / 'b / "C" / "D" / 'e / 'f / "G" / 'h / "I"
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
    val A = Root / 'a
    assertEquals(None, A.unapply("/"))
    assertEquals(None, A.unapply("/A/B"))
  }

  @Test
  def failMixed{
    val A = Root / 'unknown / "A"
    assertEquals(None, A.unapply("/"))
    assertEquals(None, A.unapply("/A/B"))
    assertEquals(None, A.unapply("/B/A/C"))
  }

  @Test
  def literalComposite {
    val AB = Root / "A" | Root / "B"
    assertEquals(AB(), "/A")
    val AB() = "/A"
    val AB() = "/B"
  }

  @Test
  def variableComposite {
    val AB = Root / 'b / "A" | Root / "A" / 'b
    assertEquals(AB("B"), "/B/A")
    val AB("B") = "/B/A"
    val AB("B") = "/A/B"
  }

  @Test
  def literalOnUnion {
    val ABC = (Root / 'b / "A" | Root / "A" / 'b) / "C"
    assertEquals(ABC("B"), "/B/A/C")
    val ABC("B") = "/B/A/C"
    val ABC("B") = "/A/B/C"
  }

  @Test
  def variableOnUnion {
    val ABX = (Root / 'x / "A" | Root / "A" / 'x) / 'y
    assertEquals(ABX("X", "Y"), "/X/A/Y")
    val ABX("X", "Y") = "/X/A/Y"
    val ABX("X", "Y") = "/A/X/Y"
  }

  @Test
  def unionsInUnions {
    val ABX = (Root / 'x / "A" | Root / "A" / 'x) / 'y
    val BYZ = (Root / "Y" / "Y" / "Y" / 'x | Root / "Y" / 'x) / "Z" / 'y / "Z"
    val XXX = (ABX | BYZ) / "U" / 'z
    assertEquals(XXX("x", "y", "z"), "/x/A/y/U/z")
    val XXX("x", "y", "z") = "/x/A/y/U/z"
    val XXX("x", "y", "z") = "/A/x/y/U/z"
  }

  @Test
  def unionBacktracking {
    val X = (Root | Root / "a") / 'b
    val X("b") = "/a/b"
  }

  @Test
  def parts {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    assertEquals(X.parts, Stream(
      Vector(Literal("a"), Var("a"), Literal("x"), Var("x")),
      Vector(Var("a"), Literal("a"), Literal("x"), Var("x"))))
  }

  @Test
  def toStrings {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    assertEquals(X.toStrings, Stream(
      "/a/{a}/x/{x}", "/{a}/a/x/{x}"))
  }

  @Test
  def checkToString {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    assertEquals(X.toString, "/a/{a}/x/{x}")
  }

  @Test
  def templates {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    def rails(s:String) = ":"+s
    assertEquals(X.templates(rails), Stream(
      "/a/:a/x/:x", "/:a/a/x/:x"))
  }

  @Test
  def template = {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    def rails(s:String) = ":"+s
    assertEquals(X.template(rails), "/a/:a/x/:x")
  }
}
