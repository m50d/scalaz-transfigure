package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import scala.annotation._
import scalaz.Id._
import scalaz.Leibniz
import scala.language.higherKinds

object TransfigureToSyntax {
  class PartialApply[Idx <: HList, AA](inner: IndexedApplyBind[Idx], f: AA) {
    @implicitNotFound("Can't combine $AA and $BB in context stack $Idx")
    def apply[A, L <: HList, LCS0 <: Context, BB, R <: HList, RCS0 <: Context, B](g: A ⇒ BB)(implicit sh1: StackHelper.Aux[Idx, AA, A, L, LCS0],
      sh2: StackHelper.Aux1[Idx, BB, R, RCS0], ab: ApplyBind[Idx, L, R] {
        type LCS = LCS0
        type RCS = RCS0
      }): ab.OCS#C[sh2.A] = inner.partialApply(f).apply(g)
  }

  class MapSugar[Idx <: HList, AA, A, L <: HList, LCS0 <: Context](a0: AA)(implicit val sh1: StackHelper.Aux[Idx, AA, A, L, LCS0]) extends ConcreteFunctorStackStack[Idx, A, L] {
    type CS = LCS0
    val fs = sh1.fs
    val a = Leibniz.witness(sh1.l)(a0)
  }

  class PartialApply1[C0 <: Context, AA](inner: IndexedApplyBind[C0 :: HNil], f: AA) extends PartialApply[C0 :: HNil, AA](inner, f)
  class PartialApply2[C0 <: Context, C1 <: Context, AA](inner: IndexedApplyBind[C0 :: C1 :: HNil], f: AA) extends PartialApply[C0 :: C1 :: HNil, AA](inner, f)
  class PartialApply3[C0 <: Context, C1 <: Context, C2 <: Context, AA](inner: IndexedApplyBind[C0 :: C1 :: C2 :: HNil], f: AA)
    extends PartialApply[C0 :: C1 :: C2 :: HNil, AA](inner, f)

  class MapSugar1[C0 <: Context, AA, A, L <: HList, LCS0 <: Context](a: AA)(implicit sh1: StackHelper.Aux[C0 :: HNil, AA, A, L, LCS0])
    extends MapSugar[C0 :: HNil, AA, A, L, LCS0](a)

  class MapSugar2[C0 <: Context, C1 <: Context, AA, A, L <: HList, LCS0 <: Context](a: AA)(
    implicit sh1: StackHelper.Aux[C0 :: C1 :: HNil, AA, A, L, LCS0]) extends MapSugar[C0 :: C1 :: HNil, AA, A, L, LCS0](a)

  class MapSugar3[C0 <: Context, C1 <: Context, C2 <: Context, AA, A, L <: HList, LCS0 <: Context](a: AA)(
    implicit sh1: StackHelper.Aux[C0 :: C1 :: C2 :: HNil, AA, A, L, LCS0]) extends MapSugar[C0 :: C1 :: C2 :: HNil, AA, A, L, LCS0](a)

  implicit class Transfigurable[A](a: A) {
    def transfigureTo[S0[_]] = new PartialApply1(ApplyBind.forIdx[Context.Aux[S0] :: HNil], a)
    def transfigureTo[S0[_], S1[_]] = new PartialApply2(ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: HNil], a)
    def transfigureTo[S0[_], S1[_], S2[_]] = new PartialApply3(ApplyBind.forIdx[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil], a)

    def mapWith[S0[_]](implicit sh: StackHelper[Context.Aux[S0] :: HNil, A]) =
      new MapSugar1[Context.Aux[S0], A, sh.A, sh.S, sh.CS](a)(sh)

    def mapWith[S0[_], S1[_]](implicit sh: StackHelper[Context.Aux[S0] :: Context.Aux[S1] :: HNil, A]) =
      new MapSugar2[Context.Aux[S0], Context.Aux[S1], A, sh.A, sh.S, sh.CS](a)(sh)

    def mapWith[S0[_], S1[_], S2[_]](implicit sh: StackHelper[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil, A]) =
      new MapSugar3[Context.Aux[S0], Context.Aux[S1], Context.Aux[S2], A, sh.A, sh.S, sh.CS](a)(sh)
  }
}