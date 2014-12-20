package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import shapeless.nat._
import shapeless.ops.nat._
import shapeless.ops.hlist._
import shapeless.test._
import scalaz._
import scalaz.Scalaz._
import TransfigureToSyntax._
import Aliases._
import org.junit.runner.RunWith
import scala.Right
import scalaz.std.either.eitherMonad
import org.junit.Test
import org.fest.assertions.Assertions.assertThat

object Aliases {
  type EitherR[A] = Either[Unit, A]
  type ValidationS[A] = Validation[String, A]
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

class IndexOfCompileTest {
  val p = IndexOf[String :: Int :: Long :: HNil, Int]
  implicitly[p.Out =:= _1]
}

class LEEqIndexedCompileTest {
  implicitly[LEIndexed[Int :: String :: HNil, String, Int]]
}

class SelectionStepTest {
  @Test def optionList: Unit =
    assertThat(SelectionStep[Idx, OptionContext, ListContext].trans(Some(List(5)))) isEqualTo Some(List(5))

  @Test def listOption: Unit =
    assertThat(SelectionStep[Idx, ListContext, OptionContext].trans(List(Some(5)))) isEqualTo Some(List(5))
}

class SelectLeastTest {
  @Test def list: Unit = {
    val sl = SelectLeast.selectLeast[ListContext :: HNil, ListContext :: HNil]
    assertThat(sl.apply(List(5))) isEqualTo List(5)
  }

  @Test def listOption: Unit = {
    val sl = SelectLeast.selectLeast[OptionContext :: ListContext :: HNil, ListContext :: OptionContext :: HNil]
    assertThat(sl.apply(List(Some(5)))) isEqualTo Some(List(5))
  }
  @Test def optionList: Unit = {
    val sl = SelectLeast.selectLeast[OptionContext :: ListContext :: HNil, OptionContext :: ListContext :: HNil]
    assertThat(sl.apply(Some(List(5)))) isEqualTo Some(List(5))
  }

  @Test def listEitherOption: Unit = {
    val sl = SelectLeast.selectLeast[OptionContext :: EitherRContext :: ListContext :: HNil, ListContext :: OptionContext :: EitherRContext :: HNil]
    assertThat(sl.apply(List(Some(Right(5))))) isEqualTo Some(List(Right(5)))
  }
}

class SelectionSortTest {
  implicitly[SelectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, HNil]]
  SelectionSort.cons[OptionContext :: EitherRContext :: ListContext :: HNil, OptionContext :: HNil, OptionContext, HNil, Context.Aux[Id], Context.Aux[Id]]

  @Test def nil: Unit = {
    val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, HNil]
    assertThat(ss.apply(5)) isEqualTo 5
  }
  @Test def reallynil: Unit = {
    val ss = SelectionSort.selectionSort[HNil, HNil]
    assertThat(ss.apply(5)) isEqualTo 5
  }
  @Test def option: Unit = {
    val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, OptionContext :: HNil]
    assertThat(ss.apply(Some(5))) isEqualTo Some(5)
  }
  @Test def listEitherOption: Unit = {
    val ss = SelectionSort.selectionSort[OptionContext :: EitherRContext :: ListContext :: HNil, ListContext :: OptionContext :: EitherRContext :: HNil]
    assertThat(ss.apply(List(Some(Right(5))))) isEqualTo Some(Right(List(5)))
  }
  @Test def optionOption: Unit = {
    val ss = SelectionSort.selectionSort[OptionContext :: HNil, OptionContext :: OptionContext :: HNil]
    assertThat(ss.apply(Some(Some(5)))) isEqualTo Some(Some(5))
  }
}

class NormalizerCompileTest {
  implicitly[Normalizer[OptionContext :: HNil, OptionContext :: OptionContext :: HNil]]
  implicitly[Normalizer[OptionContext :: ListContext :: HNil, OptionContext :: OptionContext :: HNil]]
}

class SortAndNormalizerTest {
  val ss = SelectionSort[OptionContext :: ListContext :: HNil, OptionContext :: OptionContext :: HNil]
  implicitly[=:=[ss.ICS, OptionOptionContext]]
  implicitly[=:=[ss.O, OptionContext :: OptionContext :: HNil]]

  @Test def nil: Unit = {
    val sn = SortAndNormalizer[OptionContext :: ListContext :: HNil, HNil]
    assertThat(sn.trans.apply(5)) isEqualTo Some(List(5))
  }
  @Test def listOption: Unit = {
    val sn = SortAndNormalizer[OptionContext :: ListContext :: HNil, ListContext :: OptionContext :: HNil]
    assertThat(sn.trans.apply(List(Some(5)))) isEqualTo Some(List(5))
  }
  @Test def optionOption: Unit = {
    val sn = SortAndNormalizer[OptionContext :: ListContext :: HNil, OptionContext :: OptionContext :: HNil]
    assertThat(sn.trans.apply(Some(Some(5)))) isEqualTo Some(List(5))
  }
}

class TransfigureTest {

  @Test def map: Unit = {
    val fa: Option[Int] = Some(42)
    val f: Int ⇒ String = _.toString

    assertThat(fa.transfigureTo[Option](f)) isEqualTo Some("42")
  }

  @Test def mapEither: Unit = {
    import scalaz.std.either._

    val fa: EitherR[Int] = Right(42)
    val f: Int ⇒ String = _.toString

    assertThat(fa.transfigureTo[EitherR](f)) isEqualTo Right("42")
  }

  @Test def flatMap: Unit = {
    val fa: Option[Int] = Some(42)
    val f: Int ⇒ Option[String] = x ⇒ Some((x - 10).toString)

    assertThat(fa.transfigureTo[Option](f)) isEqualTo Some("32")
  }

  @Test def join: Unit = {
    val fa: Option[Option[Int]] = Some(Some(42))
    val f: Int ⇒ Int = _ + 1

    assertThat(fa.transfigureTo[Option](f)) isEqualTo Some(43)
  }

  @Test def point: Unit = {
    val fa: Option[Int] = Some(42)
    val f: Int ⇒ String = _.toString

    assertThat(fa.transfigureTo[List, Option](f)) isEqualTo List(Some("42"))
  }

  @Test def traverse: Unit = {
    val fa: Option[Int] = Some(42)
    val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)

    assertThat(fa.transfigureTo[List, Option](f)) isEqualTo List(Some("32"))
  }

  @Test def bindTraverse: Unit = {
    val fa: List[Option[Int]] = List(Some(42))
    val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)

    assertThat(fa.transfigureTo[List, Option](f)) isEqualTo List(Some("32"))
  }

  @Test def traverseJoin: Unit = {
    val fa: List[Option[Int]] = List(Some(42))
    val f: Int ⇒ List[Option[String]] = x ⇒ List(Some((x - 10).toString))

    assertThat(fa.transfigureTo[List, Option](f)) isEqualTo List(Some("32"))
  }

  @Test def mapMap: Unit = {
    val fa: List[Option[Int]] = List(Some(42))
    val f: Int ⇒ String = _.toString

    assertThat(fa.transfigureTo[List, Option](f)) isEqualTo List(Some("42"))
  }

  @Test def mapFlatMap: Unit = {
    val fa: List[Option[Int]] = List(Some(42))
    val f: Int ⇒ Option[String] = x ⇒ Some((x - 10).toString)

    assertThat(fa.transfigureTo[List, Option](f)) isEqualTo List(Some("32"))
  }

  @Test def mapMapMap: Unit = {
    import scalaz.std.either._
    val fa: EitherR[List[Option[Int]]] = Right(List(Some(2)))
    val f: Int ⇒ Int = x ⇒ x + 2

    assertThat(fa.transfigureTo[EitherR, List, Option](f)) isEqualTo Right(List(Some(4)))
  }

  @Test def flatMapMapFlatMap: Unit = {
    import scalaz.std.either._
    val fa: EitherR[List[Option[Int]]] = Right(List(Some(2)))
    val f: Int ⇒ EitherR[Option[Int]] = x ⇒ Right(Some(x + 2))

    assertThat(fa.transfigureTo[EitherR, List, Option](f)) isEqualTo Right(List(Some(4)))
  }

  @Test def functor: Unit = {
    val fa: Int = 4
    val f: Int ⇒ ValidationS[Int] = x ⇒ (x - 1).success

    assertThat(fa.transfigureTo[ValidationS](f)) isEqualTo 3.success
  }

  @Test def mapFunctor: Unit = {
    val fa: ValidationS[Int] = 5.success
    val f: Int ⇒ Int = x ⇒ x + 3

    assertThat(fa.transfigureTo[ValidationS](f)) isEqualTo 8.success
  }

  @Test def pointFunctor: Unit = {
    val fa: Int = 4
    val f: Int ⇒ Int = x ⇒ x + 2

    assertThat(fa.transfigureTo[ValidationS](f)) isEqualTo 6.success
  }

  @Test def distribute: Unit = {
    val fa: Name[Int] = Name(5)
    val f: Int ⇒ IntReader[Int] = i ⇒ Reader(j ⇒ i + j)

    assertThat(fa.transfigureTo[IntReader, Name](f).run(4).value) isEqualTo 9
  }

  @Test def sugar: Unit = {
    import scalaz.std.either._
    val fa: EitherR[List[Option[Int]]] = Right(List(Some(2)))
    val f: Int ⇒ EitherR[Option[Float]] = x ⇒ Right(Some(x + 2.0f))
    val g: Float ⇒ List[Float] = x ⇒ List(x, x, x)
    val h: Float ⇒ Option[String] = x ⇒ Some(x.toString)

    for {
      a ← fa.mapWith[EitherR]
    } yield a

    val fb = fa.mapWith[EitherR, List, Option].flatMap(f).flatMap(g).flatMap(h).map(identity)
    //    (for {
    //      a ← fa.mapWith[EitherR, List, Option]
    //      b ← f(a)
    //      c ← g(b)
    //      d ← h(c)
    //    } yield d) 
    assertThat(fb) isEqualTo Right(List(Some("4.0"), Some("4.0"), Some("4.0")))
  }

  @Test def ignoreUnindexed: Unit = {
    val fa: Option[List[Int]] = Some(List(42))
    val f: List[Int] ⇒ List[String] = _.map(_.toString)

    assertThat(fa.transfigureTo[Option](f)) isEqualTo Some(List("42"))
  }
}

