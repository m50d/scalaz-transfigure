package scalaz

import scala.annotation._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object TransfigureToMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val input = annottees.map(_.tree).toList.head
    val unapplyTrait = q"""trait UnapplyS0[S0[_], A, F, B] {
    def apply(a: A)(f: F): B
}"""
    val ClassDef(_, unapplyTraitName, tparams, _) = unapplyTrait

    val List(a, f, b) = tparams.takeRight(3)

    val i0Name = TypeName("UnapplyS0I0")

    val i0 = q"""trait $i0Name {
	def fromFunction[S0[_], A, F, B](x: A ⇒ F ⇒ B) = new ${unapplyTraitName}[S0, A, F, B] {
    def apply(a: A)(f: F): B = x(a)(f)
  }
}"""
    val i1Name = TypeName("UnapplyS0I1")
    val i1 = q""" trait $i1Name extends $i0Name {
  implicit def join[S0[_], A, B](implicit ts: Transfigure[({type L[Z] = S0[S0[Z]]})#L, S0, Id]): ${unapplyTraitName}[S0, S0[S0[A]], A ⇒ B, S0[B]] =
      fromFunction(ts.transfigure)
}
"""
    val i2Name = TypeName("UnapplyS0I2")
    val i2 = q"""trait $i2Name extends $i1Name {
implicit def map[S0[_], A, B](implicit ts: Transfigure[S0, S0, Id]): ${unapplyTraitName}[S0, S0[A], A ⇒ B, S0[B]] =
      fromFunction(ts.transfigure)
}"""

    val i3Name = TypeName("UnapplyS0I3")
    val i3 = q"""trait $i3Name extends $i2Name {
    implicit def flatMap[S0[_], A, B](implicit ts: Transfigure[S0, S0, S0]): ${unapplyTraitName}[S0, S0[A], A ⇒ S0[B], S0[B]] =
      fromFunction(ts.transfigure)
}"""

    //splice the new traits into the object
    val ModuleDef(modifiers, termName, template) = input
    val Template(parents, self, body) = template
    val splicedBody = body :+ unapplyTrait :+ i0 :+ i1 :+ i2 :+ i3
    val splicedTemplate = Template(parents, self, splicedBody)
    val output = ModuleDef(modifiers, termName, splicedTemplate)

    println(output)
    c.Expr[Any](output)
  }
}

class TransfigureToMacro extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro TransfigureToMacro.impl
}