
package object linx {
  implicit class SymbolSyntax(val symbol:Symbol) extends AnyVal {
    def apply[A : TypeSymbol] = new TypedSymbol[A](symbol)
  }
}
