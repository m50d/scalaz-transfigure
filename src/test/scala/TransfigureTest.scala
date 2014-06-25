package scalaz

import org.specs2._

import scalaz.Id._
import scalaz.Transfigure._

import scalaz.std.option._
import scalaz.std.list._

// TODO Avoid the need of `Id` in `map` and `map.map`.

class TransfigureSpec extends mutable.Specification {

  type LO[A] = List[Option[A]]
  type LLO[A] = List[List[Option[A]]]

  "Transfigure" should {
    "map" in {
      val fa: Option[Int] = Some(42)
      val f: Int => String = _.toString

      fa.transfigureTo[Option].map(f) mustEqual Some("42")
    }

    "flatMap" in {
      val fa: Option[Int] = Some(42)
      val f: Int => Option[String] = x => Some((x - 10).toString)

      fa.transfigureTo[Option].flatMap(f) mustEqual Some("32")
    }

    "traverse" in {
      val fa: Option[Int] = Some(42)
      val f: Int => List[String] = x => List((x - 10).toString)

      fa.transfigureTo[LO].flatMap(f) mustEqual List(Some("32"))
    }

    "traverse.join" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int => List[Option[String]] = x => List(Some((x - 10).toString))

      fa.transfigureTo[LO].flatMap1(f) mustEqual List(Some("32"))
    }

    "map.map" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int => String = _.toString

      fa.transfigureTo[LO].map(f) mustEqual List(Some("42"))
    }

    "map.map.map" in {
      val fa: List[List[Option[Int]]] = List(List(Some(42)))
      val f: Int => String = _.toString

      fa.transfigureTo[LLO].map(f) mustEqual List(List(Some("42")))
    }

    "map.flatMap" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int => Option[String] = x => Some((x - 10).toString)

      fa.transfigureTo[LO].flatMap(f) mustEqual List(Some("32"))
    }
  }
}

