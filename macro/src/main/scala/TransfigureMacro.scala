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
    //    val ClassDef(mods, name, tparams, impl) = input
    //    val relevantTparams = tparams.dropRight(3)
    //    val List(s0) = relevantTparams
    //    val baseName = name.decodedName.toString
    val unapplyTrait = q"""trait UnapplyS0[S0[_], A, F, B] {
    def apply(a: A)(f: F): B
}"""
    val ClassDef(_, _, tparams, _) = unapplyTrait
    val i0Name = TypeName("UnapplyS0I0")

    val fromFunctionBody = q"new ${input} {def apply(a: A)(f: F): B = x(a)(f) }"

    val i0 = q"""trait $i0Name {
	def fromFunction[${tparams: _*}] = new ${unapplyTrait} {
    def apply(a: A)(f: F): B = x(a)(f)
  }
}"""

    //splice the new traits into the object
    val ModuleDef(modifiers, termName, template) = input
    val Template(parents, self, body) = template
    val splicedBody = body :+ unapplyTrait //:+ i0
    val splicedTemplate = Template(parents, self, splicedBody)
    val output = ModuleDef(modifiers, termName, splicedTemplate)

    println(output)
    c.Expr[Any](output)
  }
}

class TransfigureToMacro extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro TransfigureToMacro.impl
}