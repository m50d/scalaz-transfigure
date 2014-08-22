package scalaz

import scala.annotation._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object TransfigureToMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val input = annottees.map(_.tree).toList.head
//    val (annottee, expandees) = inputs match {
//      case (param: ValDef) :: (rest @ (_ :: _)) ⇒ (param, rest)
//      case (param: TypeDef) :: (rest @ (_ :: _)) ⇒ (param, rest)
//      case _ ⇒ (EmptyTree, inputs)
//    }
//    println((annottee, expandees))
//    val outputs = expandees
    val ClassDef(mods, name, tparams, impl) = input
    println(tparams.size)
    throw new RuntimeException(s"In macro; size=${tparams.size}")
    c.Expr[Any](input)
  }
}

class TransfigureToMacro extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TransfigureToMacro.impl
}