package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import scala.annotation._
import scalaz.Id._

object TransfigureToSyntax {
  implicit class Transfigurable[A, A1, S1 <: HList, CS1 <: Context](a: A)(implicit val sh: StackHelper[A]{type A = A1; type S = S1; type CS = CS1}) {
    def transfigureTo1[S0[_]] = ApplyBind.forIdx[Context.Aux[S0] :: HNil].partialApply(a)
    def transfigureTo2[S0[_], S1[_]] = ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: HNil].partialApply(a)
    def transfigureTo3[S0[_], S1[_], S2[_]] = ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil].partialApply(a)
  }
}