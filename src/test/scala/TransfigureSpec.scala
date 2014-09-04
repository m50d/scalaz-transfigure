package scalaz.transfigure

import org.specs2._
import shapeless._
import nat._
import ops.nat._
import ops.hlist._
import test._
import scalaz._
import scalaz.Scalaz._

object Aliases {
  type EitherR[A] = Either[Unit, A]
  type IntReader[A] = scalaz.Reader[Int, A]
  type OptionContext = Context.Aux[Option]
  type ListContext = Context.Aux[List]
  type EitherRContext = Context.Aux[EitherR]
  type IntReaderContext = Context.Aux[IntReader]
  type Idx = OptionContext :: ListContext :: EitherRContext :: HNil
}

import Aliases._

class IndexOfSpec {
  val p = IndexOf[String :: Int :: Long :: HNil, Int]
  implicitly[p.Out =:= _1]
}

class LTEqIndexedSpec {
  implicitly[LTIndexed[Int :: String :: HNil, String, Int]]
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
  
  "SelectionSort" should {
    "nil" in {
      val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, HNil]
      ss.apply(5) ==== 5
    }
//    "list.either.option" in {
//      val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, ListContext :: OptionContext :: EitherRContext :: HNil]
//      sl.apply(List(Some(Right(5)))) ==== Some(Right(List(5)))
//    }
  }
}

class TransfigureSpec extends mutable.Specification {

  "Transfigure" should {

    //    "map" in {
    //      val fa: Option[Int] = Some(42)
    //      val f: Int ⇒ String = _.toString
    //
    //      fa.transfigureTo[Option](f) ==== Some("42")
    //    }
    //
    //    "map (either)" in {
    //      import scalaz.std.either._
    //
    //      val fa: EitherR[Int] = Right(42)
    //      val f: Int ⇒ String = _.toString
    //
    //      fa.transfigureTo[EitherR](f) ==== Right("42")
    //    }
    //
    //    "flatMap" in {
    //      val fa: Option[Int] = Some(42)
    //      val f: Int ⇒ Option[String] = x ⇒ Some((x - 10).toString)
    //
    //      fa.transfigureTo[Option](f) ==== Some("32")
    //    }
    //
    //    "point" in {
    //      val fa: Option[Int] = Some(42)
    //      val f: Int ⇒ String = _.toString
    //
    //      fa.transfigureTo[List, Option](f) ==== List(Some("42"))
    //    }
    //
    //    "traverse" in {
    //      val fa: Option[Int] = Some(42)
    //      val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)
    //
    //      fa.transfigureTo[List, Option](f) ==== List(Some("32"))
    //    }
    //
    //    "bind.traverse" in {
    //      val fa: List[Option[Int]] = List(Some(42))
    //      val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)
    //
    //      fa.transfigureTo[List, Option](f) ==== List(Some("32"))
    //    }
    //
    //    "traverse.join" in {
    //      val fa: List[Option[Int]] = List(Some(42))
    //      val f: Int ⇒ List[Option[String]] = x ⇒ List(Some((x - 10).toString))
    //
    //      fa.transfigureTo[List, Option](f) ==== List(Some("32"))
    //    }
    //
    //    "map.map" in {
    //      val fa: List[Option[Int]] = List(Some(42))
    //      val f: Int ⇒ String = _.toString
    //
    //      fa.transfigureTo[List, Option](f) ==== List(Some("42"))
    //    }
    //
    //    "map.flatMap" in {
    //      val fa: List[Option[Int]] = List(Some(42))
    //      val f: Int ⇒ Option[String] = x ⇒ Some((x - 10).toString)
    //
    //      fa.transfigureTo[List, Option](f) ==== List(Some("32"))
    //    }
    //
    //    "map.map.map" in {
    //      import scalaz.std.either._
    //      val fa: EitherR[List[Option[Int]]] = Right(List(Some(2)))
    //      val f: Int ⇒ Int = x ⇒ x + 2
    //
    //      fa.transfigureTo[EitherR, List, Option](f) ==== Right(List(Some(4)))
    //    }
  }
}

