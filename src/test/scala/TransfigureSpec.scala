package scalaz.transfigure

import org.specs2._
import shapeless._
import nat._
//import ops.nat._
import ops.hlist._
import test._

class IndexOfSpec {
//  "IndexOf" should {
//    "count" in {
      typed[_1]((1 :: HNil).length)
      
      val l = implicitly[Length[Int :: HNil]]
      val m = implicitly[IndexOf[Int :: HNil, Int]]
      val n = implicitly[IndexOf[Int :: Long :: HNil, Int]]
      val o = IndexOf.cons[Int, String, Int :: Long :: HNil](n)
      val p = implicitly[IndexOf[String :: Int :: Long :: HNil, Int]]
      
//      typed[_1](l())
//      typed[_1](implicitly[Length[Int :: HNil]]())
//      implicitly[c =:= _0]
//      typed[_1](Length[Int :: HNil])
//      {}
//      count === 0
//    }
//  }
  //  implicitly[Length[HNil]#Out =:= _0]
  //  implicitly[IndexOf[Int :: HNil, Int] =:= _0]
  //  implicitly[IndexOf[String :: Int :: Long :: HNil, Int]#Out =:= _2]
}

//class LTEqIndexedSpec extends mutable.Specification {
//  implicitly[LTEqIndexed[Int :: String :: HNil, Int, String]]
//}

class TransfigureSpec extends mutable.Specification {

  "Transfigure" should {

    type EitherR[A] = Either[Unit, A]
    type IntReader[A] = scalaz.Reader[Int, A]

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

