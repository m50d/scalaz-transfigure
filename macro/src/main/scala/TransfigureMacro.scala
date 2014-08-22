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
    val relevantTparams = tparams.dropRight(3)
    val List(s0) = relevantTparams
    val baseName = name.decodedName.toString
    val i0 = newTypeName(baseName + "I0")
    
    val fromFunctionBody = q"new ${input} {def apply(a: A)(f: F): B = x(a)(f) }"
    
//    val fromFunctionDef = 
    
//    val t = ClassDef(Modifiers(TRAIT), i0, List())
    
    val r = q"""trait $i0 {
	def fromFunction[${tparams}] = new ${input} {
    def apply(a: A)(f: F): B = x(a)(f)
  }
}"""
    //    throw new RuntimeException(s"In macro; size=${tparams.size}")
    c.Expr[Any](r)
  }
}

class TransfigureToMacro extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro TransfigureToMacro.impl
}