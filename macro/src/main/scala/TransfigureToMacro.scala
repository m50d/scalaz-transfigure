package scalaz

import scala.annotation._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

object TransfigureToMacro {
  private[scalaz] def sublistsOfSize[A](n: Int)(l: List[A]): List[List[A]] =
    if (0 == n) List(List())
    else if (n > l.size) List()
    else {
      val hd :: tl = l
      sublistsOfSize(n)(tl) ::: (sublistsOfSize(n - 1)(tl) map { hd :: _ })
    }
  private[scalaz] def sublistPairs[A](l: List[A]) = (for {
    totalSize ← 0 to (l.size * 2)
    rightSize ← 0 to totalSize
    leftSize = totalSize - rightSize
    leftList ← sublistsOfSize(leftSize)(l)
    rightList ← sublistsOfSize(rightSize)(l)
  } yield (leftList, rightList)) toList

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val input = annottees.map(_.tree).toList.head
    val ModuleDef(modifiers, termName, template) = input
    val Template(parents, self, body) = template
    val traitsAndCompanions = for {
      unapply ← body.collect { case cd: ClassDef ⇒ cd }
      ClassDef(_, unapplyName, tparams, _) = unapply
      (contexts, List(a, f, b)) = tparams.splitAt(tparams.size - 3)
      name = { x: Int ⇒ TypeName(s"${unapplyName.decodedName.toString}I$x") }

      _ = contexts.toSet.subsets
      i0Name = name(0)
      
      functionName = TermName("fromFunction")
//      innerTypeParameter = TypeDef(newTypeParameter(TypeName("_")))
//      typeParameters = List(TypeDef(newTypeParameter(TypeName("S0")), List(innerTypeParameter)),
//          TypeDef(newTypeParameter(TypeName("A"))), TypeDef(newTypeParameter(TypeName("F"))), TypeDef(newTypeParameter(TypeName("B"))))

      ai = tq"A"
      Ident(aname: TypeName) = ai
      
      i0 = q"""trait $i0Name {
	def fromFunction[S0[_], $aname, F, B](x: $ai ⇒ F ⇒ B) = new ${unapplyName}[S0, $ai, F, B] {
    def apply(a: $ai)(f: F): B = x(a)(f)
  }
}"""
      i2Name = name(1)
      i2 = q"""trait $i2Name extends $i0Name {
implicit def map[S0[_], A, B](implicit ts: Transfigure[S0, S0, Id]): ${unapplyName}[S0, S0[A], A ⇒ B, S0[B]] =
      fromFunction(ts.transfigure)
}"""

      i3Name = name(2)
      i3 = q"""trait $i3Name extends $i2Name {
    implicit def flatMap[S0[_], A, B](implicit ts: Transfigure[S0, S0, S0]): ${unapplyName}[S0, S0[A], A ⇒ S0[B], S0[B]] =
      fromFunction(ts.transfigure)
}"""

      companionName = unapplyName.toTermName
      unapplyObject = q"""object $companionName extends $i3Name
"""
      traitOrCompanion ← List(i0, i2, i3, unapplyObject)
    } yield traitOrCompanion

    //splice the new traits into the object
    val splicedBody = body ++ traitsAndCompanions
    val splicedTemplate = Template(parents, self, splicedBody)
    val output = ModuleDef(modifiers, termName, splicedTemplate)

    println(output)
    c.Expr[Any](output)
  }
}

class TransfigureToMacro extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro TransfigureToMacro.impl
}