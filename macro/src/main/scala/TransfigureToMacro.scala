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

      functionName = TermName("fromFunction")
      contextIds = (0 until contexts.size).toList
      contextNames = contextIds map { x ⇒ TypeName(s"S$x") }
      contextTrees = contextNames map {
        cn ⇒
          TypeDef(Modifiers(Flag.PARAM), cn, List(TypeDef(Modifiers(Flag.PARAM), typeNames.WILDCARD, List(),
            TypeBoundsTree(TypeTree(), TypeTree()))), TypeBoundsTree(TypeTree(), TypeTree()))
      }
      aname = TypeName("A")
      atree = TypeDef(Modifiers(Flag.PARAM), aname, List(), TypeBoundsTree(TypeTree(), TypeTree()))
      fname = TypeName("F")
      ftree = TypeDef(Modifiers(Flag.PARAM), fname, List(), TypeBoundsTree(TypeTree(), TypeTree()))
      bname = TypeName("B")
      btree = TypeDef(Modifiers(Flag.PARAM), bname, List(), TypeBoundsTree(TypeTree(), TypeTree()))

      baseCompanionName = name(0)
      baseCompanion = q"""trait $baseCompanionName {
	def fromFunction[..${contextTrees :+ atree :+ ftree :+ btree}](x: ${Ident(aname)} ⇒ ${Ident(fname)} ⇒ ${Ident(bname)}) =
    new ${unapplyName}[..${contextNames :+ aname :+ fname :+ bname}] {
      def apply(a: $aname)(f: $fname): $bname = x(a)(f)
  }
}""";

      (currentName, companions) = ((baseCompanionName, List(baseCompanion)) /: sublistPairs(contextIds).zipWithIndex) {
        case ((lastName, lastCompanions), ((leftContexts, rightContexts), i)) ⇒
          val currentName = name(i + 1)
          val methodName = TermName(s"generated$i")
          val currentCompanion = q"""trait $currentName extends $lastName {
implicit def methodNameTODO[..${contextNames :+ aname :+ bname}](implicit ts: Transfigure[S0, S0, Id])
}"""
//	implicit def methodNameTODO[..${contextNames :+ aname :+ bname}](implicit ts: Transfigure[S0, S0, Id]) 

//: ${unapplyName}[S0, S0[A], A ⇒ B, S0[B]] =
//		fromFunction(ts.transfigure)
//}"""
          (currentName, lastCompanions :+ currentCompanion)
      }
      _ = println(companions)

      i2Name = name(1)
      i2 = q"""trait $i2Name extends $baseCompanionName {
implicit def map[S0[_], A, B](implicit ts: Transfigure[S0, S0, Id]): ${unapplyName}[S0, S0[A], A ⇒ B, S0[B]] =
      fromFunction(ts.transfigure)
}"""

      i3Name: TypeName = name(2)
      i3 = q"""trait $i3Name extends $i2Name {
    implicit def flatMap[S0[_], A, B](implicit ts: Transfigure[S0, S0, S0]): ${unapplyName}[S0, S0[A], A ⇒ S0[B], S0[B]] =
      fromFunction(ts.transfigure)
}"""

      companionName: TermName = unapplyName.toTermName
      companionObject = q"""object $companionName extends $i3Name"""
      traitOrCompanion ← List(baseCompanion, i2, i3, companionObject)
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