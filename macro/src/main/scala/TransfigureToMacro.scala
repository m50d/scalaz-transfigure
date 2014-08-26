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

      contextIds = (0 until contexts.size).toList
      contextNames = contextIds map { x ⇒ TypeName(s"S$x") }
      contextTrees = contextNames map {
        cn ⇒
          TypeDef(Modifiers(Flag.PARAM), cn, List(TypeDef(Modifiers(Flag.PARAM), typeNames.WILDCARD, List(),
            TypeBoundsTree(TypeTree(), TypeTree()))), TypeBoundsTree(TypeTree(), TypeTree()))
      }
      //Converts a list of contexts into the type for this stack of contexts
      //e.g. List(0, 2) => ({type L[A] = S0[S2[A]]})#L
      contextsType: PartialFunction[List[Int], TypeName] = {
        case Nil ⇒ TypeName("Id")
        case List(s0) ⇒ contextNames(s0)
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
          val lhsType = tq"${contextsType(leftContexts)}[${aname}]"
          val rhsType = tq"${aname} ⇒ ${contextsType(rightContexts)}[${bname}]"
          val resultType = tq"${contextsType(contextIds)}[${bname}]"
          val currentCompanion = q"""trait $currentName extends $lastName {
implicit def ${methodName}[..${contextTrees :+ atree :+ btree}]
(implicit ts: Transfigure[${contextsType(leftContexts)}, ${contextsType(contextIds)}, ${contextsType(rightContexts)}])
  : ${unapplyName}[..${{contextNames map {Ident(_)}} :+ lhsType :+ rhsType :+ resultType}] = fromFunction(ts.transfigure)  
}"""
          (currentName, lastCompanions :+ currentCompanion)
      }

      companionName: TermName = unapplyName.toTermName
      companionObject = q"""object $companionName extends $currentName"""
      traitOrCompanion ← companions :+ companionObject
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