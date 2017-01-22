package linx

import org.scalatest._

class LinxTest extends FunSuite {
  def assertEquals[X](x: X, y: X) = assert(x === y)
  def assertFalse(boolean: Boolean) = assert(!boolean)

  test("empty") {
    val path = "/"
    assertEquals(Root(), path)
    val Root() = path
  }

  test("variable") {
    val X = Root / 'x
    val path = "/x"
    val X(x) = path
    assertEquals("x", x)
    assertEquals(X(x), path)
  }

  test("variableMulti") {
    val X = Root / 'a / 'b / 'c
    val path = "/a/b/c"
    val X(a,b,c) = path
    assertEquals("a", a)
    assertEquals("b", b)
    assertEquals("c", c)
    assertEquals(path, X(a,b,c))
  }

  test("literal") {
    val X = Root / "A" / "B" / "C"
    val path = "/A/B/C"
    assertEquals(path, X())
    val X() = path
  }

  test("mixed") {
    val X = Root / "A" / 'b / "C" / "D" / 'e / 'f / "G" / 'h / "I"
    val path = "/A/B/C/D/E/F/G/H/I"
    val X(b, e, f, h) = path
    assertEquals("B", b)
    assertEquals("E", e)
    assertEquals("F", f)
    assertEquals("H", h)
    assertEquals(path, X(b, e, f, h))
  }

  test("failRoot") {
    assertFalse(Root.unapply("/a"))
  }

  test("failLiteral") {
    val A = Root / "A"
    assertFalse(A.unapply("/"))
    assertFalse(A.unapply("/B"))
    assertFalse(A.unapply("/A/B"))
  }

  test("failVariable") {
    val A = Root / 'a
    assertEquals(None, A.unapply("/"))
    assertEquals(None, A.unapply("/A/B"))
  }

  test("failMixed") {
    val A = Root / 'unknown / "A"
    assertEquals(None, A.unapply("/"))
    assertEquals(None, A.unapply("/A/B"))
    assertEquals(None, A.unapply("/B/A/C"))
  }

  test("literalComposite") {
    val AB = Root / "A" | Root / "B"
    assertEquals(AB(), "/A")
    val AB() = "/A"
    val AB() = "/B"
  }

  test("variableComposite") {
    val AB = Root / 'b / "A" | Root / "A" / 'b
    assertEquals(AB("B"), "/B/A")
    val AB("B") = "/B/A"
    val AB("B") = "/A/B"
  }

  test("literalOnUnion") {
    val ABC = (Root / 'b / "A" | Root / "A" / 'b) / "C"
    assertEquals(ABC("B"), "/B/A/C")
    val ABC("B") = "/B/A/C"
    val ABC("B") = "/A/B/C"
  }

  test("variableOnUnion") {
    val ABX = (Root / 'x / "A" | Root / "A" / 'x) / 'y
    assertEquals(ABX("X", "Y"), "/X/A/Y")
    val ABX("X", "Y") = "/X/A/Y"
    val ABX("X", "Y") = "/A/X/Y"
  }

  test("unionsInUnions") {
    val ABX = (Root / 'x / "A" | Root / "A" / 'x) / 'y
    val BYZ = (Root / "Y" / "Y" / "Y" / 'x | Root / "Y" / 'x) / "Z" / 'y / "Z"
    val XXX = (ABX | BYZ) / "U" / 'z
    assertEquals(XXX("x", "y", "z"), "/x/A/y/U/z")
    val XXX("x", "y", "z") = "/x/A/y/U/z"
    val XXX("x", "y", "z") = "/A/x/y/U/z"
  }

  test("unionBacktracking") {
    val X = (Root | Root / "a") / 'b
    val X("b") = "/a/b"
  }

  test("parts") {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    assertEquals(X.parts, Stream(
      Vector(Literal("a"), Var("a"), Literal("x"), Var("x")),
      Vector(Var("a"), Literal("a"), Literal("x"), Var("x"))))
  }

  test("checkToString") {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    assertEquals(X.toString, "/a/{a}/x/{x}")
  }

  test("templates") {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    def rails(s:String) = ":"+s
    assertEquals(X.templates(rails), Stream(
      "/a/:a/x/:x", "/:a/a/x/:x"))
  }

  test("template") {
    val X = (Root / "a" / 'a | Root / 'a / "a") / "x" / 'x
    def rails(s:String) = ":"+s
    assertEquals(X.template(rails), "/a/:a/x/:x")
  }

  test("lenientLiteral") {
    val X    = Root / "/A" / "B/C"
    val path = "/A/B/C"
    val X()  = path

    assertEquals(path, X())
  }

  test("lenientVariable") {
    val X    = Root / 'x / "//bar/baz///"
    val path = "/foo/bar/baz"
    val X(x) = path

    assertEquals("foo", x)
    assertEquals(X(x), path)
  }

  test("lenientUnapply") {
    val X    = Root / 'x / "//bar/baz///"
    val path = "/foo/bar/baz"
    val X(x) = path.replaceAll("/", "//")

    assertEquals("foo", x)
    assertEquals(X(x), path)
  }
}
