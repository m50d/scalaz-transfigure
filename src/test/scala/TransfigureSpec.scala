package scalaz

import org.specs2._

import scalaz.Id._
import scalaz.Transfigure._
import scalaz.TransfigureTo.syntax._

import scalaz.std.option._
import scalaz.std.list._

class TransfigureSpec extends mutable.Specification {

  "Transfigure" should {

    type EitherR[A] = Either[Unit, A]

    "map" in {
      val fa: Option[Int] = Some(42)
      val f: Int => String = _.toString

      fa.transfigureTo[Option](f) mustEqual Some("42")
    }

    "map (either)" in {
      import scalaz.std.either._

      val fa: EitherR[Int] = Right(42)
      val f: Int => String = _.toString

      fa.transfigureTo[EitherR](f) mustEqual Right("42")
    }

    "flatMap" in {
      val fa: Option[Int] = Some(42)
      val f: Int => Option[String] = x => Some((x - 10).toString)

      fa.transfigureTo[Option](f) mustEqual Some("32")
    }

    "join" in {
      val fa: Option[Option[Int]] = Some(Some(42))
      val f: Int => Int = _ + 1

      fa.transfigureTo[Option](f) mustEqual Some(43)
    }

    "traverse" in {
      val fa: Option[Int] = Some(42)
      val f: Int => List[String] = x => List((x - 10).toString)

      fa.transfigureTo[List, Option](f) mustEqual List(Some("32"))
    }

    // TODO Make those tests work again...
    /*
    "traverse.join" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int => List[Option[String]] = x => List(Some((x - 10).toString))

      fa.transfigureTo[List, Option](f) mustEqual List(Some("32"))
    }

    "map.map" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int => String = _.toString

      fa.transfigureTo[List, Option](f) mustEqual List(Some("42"))
    }

    "map.map.map" in {
      val fa: List[List[Option[Int]]] = List(List(Some(42)))
      val f: Int => String = _.toString

      fa.transfigureTo[List, List, Option](f) mustEqual List(List(Some("42")))
    }

    "map.flatMap" in {
      val fa: List[Option[Int]] = List(Some(42))
      val f: Int => Option[String] = x => Some((x - 10).toString)

      fa.transfigureTo[List, Option](f) mustEqual List(Some("32"))
    }

    "map.map.flatMap" in {
      val fa: List[List[Option[Int]]] = List(List(Some(42)))
      val f: Int => Option[String] = x => Some((x - 10).toString)

      fa.transfigureTo[List, List, Option](f) mustEqual List(List(Some("32")))
    }

    "point.map" in {
      val fa: Option[Int] = Some(42)
      val f: Int => String = _.toString

      fa.transfigureTo[List, Option].map(f) mustEqual List(Some("42"))
    }
    */
  }
}

