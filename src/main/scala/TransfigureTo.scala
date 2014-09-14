package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import scala.annotation._
import scalaz.Id._

object TransfigureToSyntax {
  implicit class Transfigurable[A](a: A) {
    def transfigureTo1[S0[_]](implicit sh: StackHelper[A]) = ApplyBind.forIdx[Context.Aux[S0] :: HNil].partialApply[A, sh.A, sh.S, sh.CS](a)(sh)
    def transfigureTo2[S0[_], S1[_]] = ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: HNil].partialApply(a)
    def transfigureTo3[S0[_], S1[_], S2[_]] = ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil].partialApply(a)
  }
}

//class TransfigureToS0[A, S0[_]](a: A) {
//  @implicitNotFound(
//    """S0
//  It's not possible to go from ${A} to ${S0} using a ${AA} => ${B}.
//  """)
//  def apply[AA, B](f: AA => B)() =
//    ApplyBind.forIdx[Context.Aux[S0] :: HNil].apply(a, f)
//}
//
//@implicitNotFound(
//  """S1
//  It's not possible to go from ${A} to ${S0}[${S1}] using ${F}.
//  """)
//class TransfigureToS1[A, S0[_], S1[_]](a: A) {
//  def apply[F, B](f: F)(implicit U: TransfigureTo.UnapplyS1[S0, S1, A, F, B]): B = U(a)(f)
//}
//
//@implicitNotFound(
//  """S2
//    It's not possible to go from ${A} to ${S0}[${S1}[${S2}]] using ${F}.
//    """)
//class TransfigureToS2[A, S0[_], S1[_], S2[_]](a: A) {
//  def apply[F, B](f: F)(implicit U: TransfigureTo.UnapplyS2[S0, S1, S2, A, F, B]): B = U(a)(f)
//}


