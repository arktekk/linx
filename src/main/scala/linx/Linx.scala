package linx

object *
object Root extends Literal(Vector.empty)

trait Linx[A, X] {
  def split(s:String) = s.split('/').toList match {
    case "" :: t => t
    case t       => t
  }
  def apply(a:A) = elements(a).mkString("/", "/", "")
  def / [B](star: *.type)(implicit param:LinxParam[A, B]) = new Variable(this, param, Vector.empty)
  def / (name:String):Linx[A, X]
  def elements(a:A): Seq[String]
  def extract(seq:List[String]):Option[(A, List[String])]
  def unapply(s:String):X
}

class Literal(parts:Vector[String]) extends Linx[Unit, Boolean]{
  def unapply(s:String) = extract(split(s)).exists(_._2.isEmpty)
  def /(name: String) = new Literal(parts :+ name)
  def elements(a: Unit) = parts
  def extract(seq: List[String]) = if (seq.startsWith(parts)) Some((), seq.drop(parts.size)) else None
}

class Variable[P, A](parent:Linx[P, _], param:LinxParam[P, A], parts:Vector[String]) extends Linx[A, Option[A]]{
  def /(name: String) = new Variable(parent, param, parts :+ name)
  def elements(a: A) = {
    val (p, part) = param.previous(a)
    parent.elements(p) ++ (part +: parts)
  }
  def extract(seq: List[String]) = for {
    (p, head :: tail) <- parent.extract(seq)
    if tail.startsWith(parts)
  } yield (param.next(p, head), tail.drop(parts.size))
  def unapply(s:String) = for { (a, Nil) <- extract(split(s)) } yield a
}

trait LinxParam[A, B]{
  def previous(b:B):(A, String)
  def next(a:A, value:String):B
}

object LinxParam {
  def apply[A, B](p:B => (A, String))(n:(A, String) => B):LinxParam[A, B] = new LinxParam[A, B] {
    def previous(b: B): (A, String) = p(b)
    def next(a: A, value: String): B = n(a, value)
  }

  implicit val param1 = LinxParam[Unit, String](((), _:String))((_, n) => n)
  implicit val param2 = LinxParam[String, (String, String)](identity)((a,b) => (a, b))
  implicit val param3 = LinxParam[(String,String), (String,String,String)]{ case (a0,a1,a2) => ((a0,a1),a2)}{ case ((a0,a1),a2) => (a0,a1,a2) }
  implicit val param4 = LinxParam[(String,String,String), (String,String,String,String)]{ case (a0,a1,a2,a3) => ((a0,a1,a2),a3)}{ case ((a0,a1,a2),a3) => (a0,a1,a2,a3) }
  implicit val param5 = LinxParam[(String,String,String,String), (String,String,String,String,String)]{ case (a0,a1,a2,a3,a4) => ((a0,a1,a2,a3),a4)}{ case ((a0,a1,a2,a3),a4) => (a0,a1,a2,a3,a4) }
  implicit val param6 = LinxParam[(String,String,String,String,String), (String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5) => ((a0,a1,a2,a3,a4),a5)}{ case ((a0,a1,a2,a3,a4),a5) => (a0,a1,a2,a3,a4,a5) }
  implicit val param7 = LinxParam[(String,String,String,String,String,String), (String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6) => ((a0,a1,a2,a3,a4,a5),a6)}{ case ((a0,a1,a2,a3,a4,a5),a6) => (a0,a1,a2,a3,a4,a5,a6) }
  implicit val param8 = LinxParam[(String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7) => ((a0,a1,a2,a3,a4,a5,a6),a7)}{ case ((a0,a1,a2,a3,a4,a5,a6),a7) => (a0,a1,a2,a3,a4,a5,a6,a7) }
  implicit val param9 = LinxParam[(String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8) => ((a0,a1,a2,a3,a4,a5,a6,a7),a8)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7),a8) => (a0,a1,a2,a3,a4,a5,a6,a7,a8) }
  implicit val param10 = LinxParam[(String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8),a9)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8),a9) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) }
  implicit val param11 = LinxParam[(String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9),a10)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9),a10) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) }
  implicit val param12 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10),a11)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10),a11) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) }
  implicit val param13 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11),a12)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11),a12) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) }
  implicit val param14 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12),a13)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12),a13) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) }
  implicit val param15 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13),a14)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13),a14) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) }
  implicit val param16 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14),a15)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14),a15) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) }
  implicit val param17 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15),a16)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15),a16) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) }
  implicit val param18 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16),a17)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16),a17) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) }
  implicit val param19 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17),a18)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17),a18) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) }
  implicit val param20 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18),a19)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18),a19) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) }
  implicit val param21 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19),a20)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19),a20) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) }
  implicit val param22 = LinxParam[(String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String), (String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String,String)]{ case (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) => ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20),a21)}{ case ((a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20),a21) => (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) }
}
