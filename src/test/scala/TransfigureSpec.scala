package scalaz.transfigure

import org.specs2._
import shapeless.{ Id ⇒ _, _ }
import nat._
import ops.nat._
import ops.hlist._
import test._
import scalaz._
import scalaz.Scalaz._
import TransfigureToSyntax._

object Aliases {
  type EitherR[A] = Either[Unit, A]
  type IntReader[A] = scalaz.Reader[Int, A]
  type OptionContext = Context.Aux[Option]
  type ListContext = Context.Aux[List]
  type EitherRContext = Context.Aux[EitherR]
  type IntReaderContext = Context.Aux[IntReader]
  type OptionOptionContext = Context {
    type C[A] = Option[Option[A]]
  }
  type Idx = OptionContext :: ListContext :: EitherRContext :: HNil
}

import Aliases._

class IndexOfSpec {
  val p = IndexOf[String :: Int :: Long :: HNil, Int]
  implicitly[p.Out =:= _1]
}

class LEEqIndexedSpec {
  implicitly[LEIndexed[Int :: String :: HNil, String, Int]]
}

class SelectionStepSpec extends mutable.Specification {

  "SelectionStep" should {
    "option.list" in {
      SelectionStep[Idx, OptionContext, ListContext].trans(Some(List(5))) ====
        Some(List(5))
    }
    "list.option" in {
      SelectionStep[Idx, ListContext, OptionContext].trans(List(Some(5))) ====
        Some(List(5))
    }
  }
}

class SelectLeastSpec extends mutable.Specification {
  "SelectLeast" should {
    "list" in {
      val sl = SelectLeast.selectLeast[ListContext :: HNil, ListContext :: HNil]
      sl.apply(List(5)) ==== List(5)
    }
    "list.option" in {
      val sl = SelectLeast.selectLeast[OptionContext :: ListContext :: HNil, ListContext :: OptionContext :: HNil]
      sl.apply(List(Some(5))) ==== Some(List(5))
    }
    "option.list" in {
      val sl = SelectLeast.selectLeast[OptionContext :: ListContext :: HNil, OptionContext :: ListContext :: HNil]
      sl.apply(Some(List(5))) ==== Some(List(5))
    }
    "list.either.option" in {
      val sl = SelectLeast.selectLeast[OptionContext :: EitherRContext :: ListContext :: HNil, ListContext :: OptionContext :: EitherRContext :: HNil]
      sl.apply(List(Some(Right(5)))) ==== Some(List(Right(5)))
    }
  }
}

class SelectionSortSpec extends mutable.Specification {
  implicitly[SelectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, HNil]]
  SelectionSort.cons[OptionContext :: EitherRContext :: ListContext :: HNil, OptionContext :: HNil, OptionContext, HNil, Context.Aux[Id], Context.Aux[Id]]

  "SelectionSort" should {
    "nil" in {
      val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, HNil]
      ss.apply(5) ==== 5
    }
    "option" in {
      val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, OptionContext :: HNil]
      ss.apply(Some(5)) ==== Some(5)
    }
    "list.either.option" in {
      val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, ListContext :: OptionContext :: EitherRContext :: HNil]
      ss.apply(List(Some(Right(5)))) ==== Some(Right(List(5)))
    }
    "option.option" in {
      val ss = SelectionSort.selectionSort[OptionContext :: HNil, OptionContext :: OptionContext :: HNil]
      ss.apply(Some(Some(5))) ==== Some(Some(5))
    }
  }
}

class NormalizerSpec extends mutable.Specification {
  implicitly[Normalizer[OptionContext :: HNil, OptionContext :: OptionContext :: HNil]]
  implicitly[Normalizer[OptionContext :: ListContext :: HNil, OptionContext :: OptionContext :: HNil]]
}

class SortAndNormalizerSpec extends mutable.Specification {
  val ss = SelectionSort[OptionContext :: ListContext :: HNil, OptionContext :: OptionContext :: HNil]
  implicitly[=:=[ss.ICS, OptionOptionContext]]
  implicitly[=:=[ss.O, OptionContext :: OptionContext :: HNil]]

  "SortAndNormalizer" should {
    "nil" in {
      val sn = SortAndNormalizer[OptionContext :: ListContext :: HNil, HNil]
      sn.trans.apply(5) ==== Some(List(5))
    }
    "list.option" in {
      val sn = SortAndNormalizer[OptionContext :: ListContext :: HNil, ListContext :: OptionContext :: HNil]
      sn.trans.apply(List(Some(5))) ==== Some(List(5))
    }
    "option.option" in {
      val sn = SortAndNormalizer[OptionContext :: ListContext :: HNil, OptionContext :: OptionContext :: HNil]
      sn.trans.apply(Some(Some(5))) ==== Some(List(5))
    }
  }
}

class ApplyBindSpec extends mutable.Specification {
  val i1 = implicitly[SelectionSort[ListContext :: HNil, HNil] {
    type ICS = Context.Aux[Id]
    type O = HNil
    type OCS = Context.Aux[Id]
  }]
  val i2 = implicitly[MonadStack[ListContext :: HNil]]
  implicitly[ApplyBind[ListContext :: HNil, HNil, HNil]]

  val ss = SelectionSort[OptionContext :: HNil, OptionContext :: OptionContext :: HNil]

  implicitly[=:=[ss.O, OptionContext :: OptionContext :: HNil]]

  //  ApplyBind.forIdx[OptionContext :: HNil].apply(Some(Some(5)): Option[Option[Int]], { x: Int ⇒ x + 1 })
  type StackedContext = Context {
    type C[A] = EitherR[List[Option[A]]]
  }
  //  implicitly[MonadStack[EitherRContext :: ListContext :: OptionContext]]
  //  ApplyBind.combine[EitherRContext :: ListContext :: OptionContext :: HNil, HNil, Context.Aux[Id], StackedContext, HNil, Context.Aux[Id], StackedContext, StackedContext]
  "ApplyBind" should {
    "nillist" in {
      ApplyBind.forIdx[ListContext :: HNil].apply(5, { x: Int ⇒ x + 1 }) ==== List(6)
    }
    "nileitherlistoption" in {
      import scalaz.std.either._
      ApplyBind.forIdx[EitherRContext :: ListContext :: OptionContext :: HNil].apply(5, { x: Int ⇒ x + 1 }) ==== Right(List(Some(6)))
    }
  }
}

class TransfigureSpec extends mutable.Specification {

  "Transfigure" should {

    "map" in {
      val fa: Option[Int] = Some(42)
      val f: Int ⇒ String = _.toString

      fa.transfigureTo1[Option](f) ==== Some("42")
    }

    "map (either)" in {
      import scalaz.std.either._

      val fa: EitherR[Int] = Right(42)
      val f: Int ⇒ String = _.toString

      fa.transfigureTo1[EitherR](f) ==== Right("42")
    }

    "flatMap" in {
      val fa: Option[Int] = Some(42)
      val f: Int ⇒ Option[String] = x ⇒ Some((x - 10).toString)

      fa.transfigureTo1[Option](f) ==== Some("32")
    }

    "join" in {
      val fa: Option[Option[Int]] = Some(Some(42))
      val f: Int ⇒ Int = _ + 1

      fa.transfigureTo1[Option](f) ==== Some(43)
    }

    "point" in {
      val fa: Option[Int] = Some(42)
      val f: Int ⇒ String = _.toString

      fa.transfigureTo2[List, Option](f) ==== List(Some("42"))
    }

    "traverse" in {
      val fa: Option[Int] = Some(42)
      val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)

      fa.transfigureTo2[List, Option](f) ==== List(Some("32"))
    }

    "bind.traverse" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)

      fa.transfigureTo2[List, Option](f) ==== List(Some("32"))
    }

    "traverse.join" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int ⇒ List[Option[String]] = x ⇒ List(Some((x - 10).toString))

      fa.transfigureTo2[List, Option](f) ==== List(Some("32"))
    }

    "map.map" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int ⇒ String = _.toString

      fa.transfigureTo2[List, Option](f) ==== List(Some("42"))
    }

    "map.flatMap" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int ⇒ Option[String] = x ⇒ Some((x - 10).toString)

      fa.transfigureTo2[List, Option](f) ==== List(Some("32"))
    }

    "map.map.map" in {
      import scalaz.std.either._
      val fa: EitherR[List[Option[Int]]] = Right(List(Some(2)))
      val f: Int ⇒ Int = x ⇒ x + 2

      fa.transfigureTo3[EitherR, List, Option](f) ==== Right(List(Some(4)))
    }

    "flatMap.map.flatMap" in {
      import scalaz.std.either._
      val fa: EitherR[List[Option[Int]]] = Right(List(Some(2)))
      val f: Int ⇒ EitherR[Option[Int]] = x ⇒ Right(Some(x + 2))

      fa.transfigureTo3[EitherR, List, Option](f) ==== Right(List(Some(4)))
    }
  }
}

