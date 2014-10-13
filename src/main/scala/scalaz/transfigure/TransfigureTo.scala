package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import scala.annotation._
import scalaz.Id._
import scala.language.higherKinds

object TransfigureToSyntax {
  class PartialApply1[S0[_], A, L <: HList, LICS <: Context](inner: PartiallyAppliedApplyBind[Context.Aux[S0] :: HNil, A, L, LICS]) {
    def apply[BB, R <: HList, RICS <: Context](g: A ⇒ BB)(implicit sh2: StackHelper.Aux1[BB, R, RICS], ab: ApplyBind[Context.Aux[S0] :: HNil, L, R] {
      type LCS = LICS
      type RCS = RICS
    }): ab.OCS#C[sh2.A] = inner.apply(g)
  }
  
  class PartialApply2[S0[_], S1[_], A, L <: HList, LICS <: Context](inner: PartiallyAppliedApplyBind[Context.Aux[S0] :: Context.Aux[S1] :: HNil, A, L, LICS]) {
    def apply[BB, R <: HList, RICS <: Context](g: A ⇒ BB)(implicit sh2: StackHelper.Aux1[BB, R, RICS], ab: ApplyBind[Context.Aux[S0] :: Context.Aux[S1] :: HNil, L, R] {
      type LCS = LICS
      type RCS = RICS
    }): ab.OCS#C[sh2.A] = inner.apply(g)
  }
  
  class PartialApply3[S0[_], S1[_], S2[_], A, L <: HList, LICS <: Context](inner: PartiallyAppliedApplyBind[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil, A, L, LICS]) {
    def apply[BB, R <: HList, RICS <: Context](g: A ⇒ BB)(implicit sh2: StackHelper.Aux1[BB, R, RICS], ab: ApplyBind[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil, L, R] {
      type LCS = LICS
      type RCS = RICS
    }): ab.OCS#C[sh2.A] = inner.apply(g)
  }

  implicit class Transfigurable[A, A1, S1 <: HList, CS1 <: Context](a: A)(implicit val sh: StackHelper[A] { type A = A1; type S = S1; type CS = CS1 }) {
    def transfigureTo[S0[_]] = new PartialApply1(transfigureTo1[S0])
    def transfigureTo[S0[_], S1[_]] = new PartialApply2(transfigureTo2[S0, S1])
    def transfigureTo[S0[_], S1[_], S2[_]] = new PartialApply3(transfigureTo3[S0, S1, S2])
    def transfigureTo1[S0[_]] = ApplyBind.forIdx[Context.Aux[S0] :: HNil].partialApply(a)
    def transfigureTo2[S0[_], S1[_]] = ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: HNil].partialApply(a)
    def transfigureTo3[S0[_], S1[_], S2[_]] = ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil].partialApply(a)
    def transfigureTo4[S0[_], S1[_], S2[_], S3[_]] = ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: Context.Aux[S3] :: HNil].partialApply(a)
    def transfigureTo5[S0[_], S1[_], S2[_], S3[_], S4[_]] =
      ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: Context.Aux[S3] :: Context.Aux[S4] :: HNil].partialApply(a)
    def transfigureTo6[S0[_], S1[_], S2[_], S3[_], S4[_], S5[_]] =
      ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: Context.Aux[S3] :: Context.Aux[S4] :: Context.Aux[S5] :: HNil].partialApply(a)
  }
}