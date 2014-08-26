package scalaz

import scala.annotation._
import scalaz.Id._
import scala.language.experimental.macros
import scala.reflect.macros.Context

trait TransfigureToSyntax {
  implicit class Transfigurable[A](a: A) {
    def transfigureTo[S0[_]] = new TransfigureToS0[A, S0](a)
    def transfigureTo[S0[_], S1[_]] = new TransfigureToS1[A, S0, S1](a)
//    def transfigureTo[S0[_], S1[_], S2[_]] = new TransfigureToS2[A, S0, S1, S2](a)
  }
}

class TransfigureToS0[A, S0[_]](a: A) {
  def apply[F, B](f: F)(implicit U: TransfigureTo.UnapplyS0[S0, A, F, B]): B = U(a)(f)
}

class TransfigureToS1[A, S0[_], S1[_]](a: A) {
  def apply[F, B](f: F)(implicit U: TransfigureTo.UnapplyS1[S0, S1, A, F, B]): B = U(a)(f)
}

//class TransfigureToS2[A, S0[_], S1[_], S2[_]](a: A) {
//  def apply[F, B](f: F)(implicit U: TransfigureTo.UnapplyS2[S0, S1, S2, A, F, B]): B = U(a)(f)
//}

@TransfigureToMacro
object TransfigureTo {
  val syntax = new TransfigureToSyntax {}

  @implicitNotFound(
    """S0
  It's not possible to go from ${A} to ${S0} using ${F}.
  """)
  trait UnapplyS0[S0[_], A, F, B] {
    def apply(a: A)(f: F): B
  }

  @implicitNotFound(
    """S1
  It's not possible to go from ${A} to ${S0}[${S1}] using ${F}.
  """)
  trait UnapplyS1[S0[_], S1[_], A, F, B] {
    def apply(a: A)(f: F): B
  }

  //    @implicitNotFound(
  //    """S2
  //  It's not possible to go from ${A} to ${S0}[${S1}[${S2}]] using ${F}.
  //  """)
  //  trait UnapplyS2[S0[_], S1[_], S2[_], A, F, B] {
  //    def apply(a: A)(f: F): B
  //  }
}


