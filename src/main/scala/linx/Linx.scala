package linx

import scala.language.implicitConversions

object Root extends StaticLinx(Vector.empty)

sealed trait Part
case class Literal(name:String) extends Part
case class Var(name:String) extends Part

object Linx {
  class VarOps[A, X](l:Linx[A, Option[X]]){
    def apply(a:A) = l.links(a).head
  }

  implicit def VarOps[A, X](l:Linx[A, Option[X]]):VarOps[A, X] = new VarOps(l)

  class NoVarOps[X](l:Linx[Unit, X]){
    def apply() = l.links(()).head
  }

  implicit def NoVarOps[X](l:Linx[Unit, X]):NoVarOps[X] = new NoVarOps(l)
}

sealed trait Linx[A, X] {
  def split(s:String) = s.split('/').toList match {
    case "" :: t => t
    case t       => t
  }

  def links(a:A) = elements(a).map(_.mkString("/", "/", ""))

  def / [B, C](s:TypedSymbol[B])(implicit param:LinxParam[A, B, C]):Linx[C, Option[C]] =
    new VariableLinx(this, param, Vector.empty, s)

  def | (or:Linx[A, X])(implicit matcher:UnapplyMatch[X]):Linx[A, X] =
    new UnionLinx(this, or, matcher)

  def / (name:String):Linx[A, X]

  def elements(a:A):Stream[Vector[String]]

  def extract(seq:List[String]):Stream[(A, List[String])]

  def unapply(s:String):X

  def parts:Stream[Vector[Part]]

  def templates(render:String => String):Stream[String] =
    parts.map(_.map {
      case Literal(l) => l
      case Var(v) => render(v)
    }.mkString("/", "/", ""))

  def template(render:String => String):String =
    templates(render).head

  // rfc6570 uri template
  override def toString = template("{" + _ + "}")
}

class StaticLinx[A](val static:Vector[String]) extends Linx[Unit, Boolean]{
  def unapply(s:String) = extract(split(s)).exists(_._2.isEmpty)

  def /(name: String) = new StaticLinx(static :+ name)

  def elements(a: Unit) = Stream(static)

  def extract(seq: List[String]) =
    if (seq.startsWith(static)) Stream(((), seq.drop(static.size))) else Stream.empty

  def parts = Stream(static.map(Literal))
}

class VariableLinx[P, A, B](parent:Linx[P, _], param:LinxParam[P, A, B], static:Vector[String], symbol:TypedSymbol[A]) extends Linx[B, Option[B]]{
  def /(name: String) = new VariableLinx(parent, param, static :+ name, symbol)

  def elements(element: B) = {
    val (p, part) = param.previous(element)
    parent.elements(p).map(_ ++ (symbol.write(part) +: static))
  }

  def extract(seq: List[String]) = for {
    (p, head :: tail) <- parent.extract(seq) if tail.startsWith(static)
    a <- symbol.read(head)
  } yield (param.next(p, a), tail.drop(static.size))

  def unapply(s:String) = (for { (a, Nil) <- extract(split(s)) } yield a).headOption

  def parts = parent.parts.map(_ ++ (Var(symbol.name) +: static.map(Literal)))
}

class UnionLinx[A, X](first:Linx[A, X], next:Linx[A, X], matcher:UnapplyMatch[X]) extends Linx[A, X]{
  def /(name: String) = new UnionLinx(first / name, next / name, matcher)

  def elements(a: A) = first.elements(a) #::: next.elements(a)

  def extract(seq: List[String]) =
    first.extract(seq) #::: next.extract(seq)

  def unapply(s: String): X = {
    val firstX = first.unapply(s)
    if(matcher.is(firstX)) firstX else next.unapply(s)
  }

  def parts = first.parts #::: next.parts
}

sealed trait UnapplyMatch[X]{
  def is(x:X):Boolean
}

object UnapplyMatch {
  implicit object boolean extends UnapplyMatch[Boolean]{
    def is(x: Boolean) = x
  }
  implicit def option[A]:UnapplyMatch[Option[A]] = new UnapplyMatch[Option[A]] {
    def is(x: Option[A]) = x.isDefined
  }
}

trait TypeSymbol[A]{
  def read(element:String):Option[A]
  def write(a:A):String
}

object TypeSymbol {
  implicit object string extends TypeSymbol[String]{
    def read(element: String) = Some(element)
    def write(a: String) = a
  }
  implicit object int extends TypeSymbol[Int]{
    def read(element: String) = util.control.Exception.allCatch.opt(element.toInt)
    def write(a: Int) = a.toString
  }
  implicit object char extends TypeSymbol[Char]{
    def read(element: String) = if(element.length == 1) Some(element.head) else None
    def write(a: Char) = a.toString
  }
}

object TypedSymbol {
  implicit def untyped(s:Symbol):TypedSymbol[String] = new TypedSymbol[String](s)
}

case class TypedSymbol[A : TypeSymbol](symbol:Symbol){
  def read(element:String):Option[A] = implicitly[TypeSymbol[A]].read(element)
  def write(a:A):String  = implicitly[TypeSymbol[A]].write(a)
  def name = symbol.name
}

trait LinxParam[A, B, C]{
  def previous(c:C):(A, B)
  def next(a:A, b:B):C
}

object LinxParam extends LowPriorityLinxParam {
  def apply[A, B, C](p:C => (A, B), n:(A, B) => C):LinxParam[A, B, C] = new LinxParam[A, B, C]{
    def previous(c: C) = p(c)
    def next(a: A, b: B) = n(a, b)
  }

  implicit def param0[A] = LinxParam[Unit, A, A]((a) => ((), a), (_, a) => a)
  // param1 has lower priority
  implicit def param2[A0, A1, A2] = LinxParam[(A0, A1), A2, (A0, A1, A2)]({ case (a0, a1, a2) => ((a0, a1), a2) },{ case ((a0, a1), a2) => (a0, a1, a2)})
  implicit def param3[A0, A1, A2, A3] = LinxParam[(A0, A1, A2), A3, (A0, A1, A2, A3)]({ case (a0, a1, a2, a3) => ((a0, a1, a2), a3) },{ case ((a0, a1, a2), a3) => (a0, a1, a2, a3)})
  implicit def param4[A0, A1, A2, A3, A4] = LinxParam[(A0, A1, A2, A3), A4, (A0, A1, A2, A3, A4)]({ case (a0, a1, a2, a3, a4) => ((a0, a1, a2, a3), a4) },{ case ((a0, a1, a2, a3), a4) => (a0, a1, a2, a3, a4)})
  implicit def param5[A0, A1, A2, A3, A4, A5] = LinxParam[(A0, A1, A2, A3, A4), A5, (A0, A1, A2, A3, A4, A5)]({ case (a0, a1, a2, a3, a4, a5) => ((a0, a1, a2, a3, a4), a5) },{ case ((a0, a1, a2, a3, a4), a5) => (a0, a1, a2, a3, a4, a5)})
  implicit def param6[A0, A1, A2, A3, A4, A5, A6] = LinxParam[(A0, A1, A2, A3, A4, A5), A6, (A0, A1, A2, A3, A4, A5, A6)]({ case (a0, a1, a2, a3, a4, a5, a6) => ((a0, a1, a2, a3, a4, a5), a6) },{ case ((a0, a1, a2, a3, a4, a5), a6) => (a0, a1, a2, a3, a4, a5, a6)})
  implicit def param7[A0, A1, A2, A3, A4, A5, A6, A7] = LinxParam[(A0, A1, A2, A3, A4, A5, A6), A7, (A0, A1, A2, A3, A4, A5, A6, A7)]({ case (a0, a1, a2, a3, a4, a5, a6, a7) => ((a0, a1, a2, a3, a4, a5, a6), a7) },{ case ((a0, a1, a2, a3, a4, a5, a6), a7) => (a0, a1, a2, a3, a4, a5, a6, a7)})
  implicit def param8[A0, A1, A2, A3, A4, A5, A6, A7, A8] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7), A8, (A0, A1, A2, A3, A4, A5, A6, A7, A8)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8) => ((a0, a1, a2, a3, a4, a5, a6, a7), a8) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7), a8) => (a0, a1, a2, a3, a4, a5, a6, a7, a8)})
  implicit def param9[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8), A9, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8), a9) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8), a9) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)})
  implicit def param10[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9), A10, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9), a10) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9), a10) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)})
  implicit def param11[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), A11, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), a11) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), a11) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)})
  implicit def param12[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), A12, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), a12) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), a12) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)})
  implicit def param13[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), A13, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), a13) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), a13) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)})
  implicit def param14[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), A14, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), a14) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), a14) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)})
  implicit def param15[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), A15, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), a15) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), a15) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)})
  implicit def param16[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), A16, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), a16) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), a16) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)})
  implicit def param17[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), A17, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16), a17) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16), a17) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)})
  implicit def param18[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), A18, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17), a18) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17), a18) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)})
  implicit def param19[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), A19, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18), a19) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18), a19) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)})
  implicit def param20[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), A20, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19), a20) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19), a20) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)})
  implicit def param21[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] = LinxParam[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), A21, (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]({ case (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) => ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20), a21) },{ case ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20), a21) => (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)})

}

trait LowPriorityLinxParam {
  // matches everything, so must have lowest priority
  implicit def param1[A0, A1] = LinxParam[A0, A1, (A0, A1)](identity, (a0, a1) => (a0, a1))
}