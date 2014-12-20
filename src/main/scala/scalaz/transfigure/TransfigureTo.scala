package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import ops.hlist.Prepend
import scala.annotation._
import scalaz.Id._
import scalaz.Leibniz
import scala.language.higherKinds

object TransfigureToSyntax {
  /**
   * TransfigureSugar delays getting the implicit StackHelper until apply time.
   * This is less good than MapSugar, but needed to make fa.transfigureTo(f) work as expected,
   * without f being interpreted as an implicit parameter.
   * MapSugar figures out the stack immediately, which means the type A is available to guide
   * inference of the next function g when we call fa.mapWith().flatMap(g).
   */
  class TransfigureSugar[Idx <: HList, AA](f: AA) {
    @implicitNotFound("Can't combine $AA and $BB in context stack $Idx")
    def apply[A, L <: HList, CS0 <: Context, BB, B, K <: HList, CS1 <: Context, KS <: HList, CS2 <: Context, CS3 <: Context](g: A ⇒ BB)(
      implicit sh1: StackHelper.Aux[Idx, AA, A, L, CS0], sh2: StackHelper.Aux[Idx, BB, B, K, CS1], pp: Prepend.Aux[L, K, KS],
      fs0: FunctorStack.Aux[KS, CS2],
      w1: Leibniz.===[CS0#C[BB], CS2#C[B]], sn: SortAndNormalizer.Aux1[Idx, KS, CS3],
      w2: LeibC[CS2, CS3]): sn.OCS#C[B] = (new ConcreteFunctorStack[Idx, A, L] {
      type CS = CS0
      val fs = sh1.fs
      val a = Leibniz.witness(sh1.l)(f)
    }).map[BB, B, K, CS1, KS, CS2, CS3](g)(sh2, pp, fs0, w1, sn, w2)
  }

  class MapSugar[Idx <: HList, AA, A, L <: HList, CS0 <: Context](a0: AA)(implicit val sh: StackHelper.Aux[Idx, AA, A, L, CS0]) extends ConcreteFunctorStack[Idx, A, L] {
    type CS = CS0
    val fs = sh.fs
    val a = Leibniz.witness(sh.l)(a0)
  }

  class TransfigureSugar1[C0 <: Context, AA](f: AA) extends TransfigureSugar[C0 :: HNil, AA](f)
  class TransfigureSugar2[C0 <: Context, C1 <: Context, AA](f: AA) extends TransfigureSugar[C0 :: C1 :: HNil, AA](f)
  class TransfigureSugar3[C0 <: Context, C1 <: Context, C2 <: Context, AA](f: AA)
    extends TransfigureSugar[C0 :: C1 :: C2 :: HNil, AA](f)

  class MapSugar1[C0 <: Context, AA, A, L <: HList, LCS0 <: Context](a: AA)(implicit sh1: StackHelper.Aux[C0 :: HNil, AA, A, L, LCS0])
    extends MapSugar[C0 :: HNil, AA, A, L, LCS0](a)

  class MapSugar2[C0 <: Context, C1 <: Context, AA, A, L <: HList, LCS0 <: Context](a: AA)(
    implicit sh1: StackHelper.Aux[C0 :: C1 :: HNil, AA, A, L, LCS0]) extends MapSugar[C0 :: C1 :: HNil, AA, A, L, LCS0](a)

  class MapSugar3[C0 <: Context, C1 <: Context, C2 <: Context, AA, A, L <: HList, LCS0 <: Context](a: AA)(
    implicit sh1: StackHelper.Aux[C0 :: C1 :: C2 :: HNil, AA, A, L, LCS0]) extends MapSugar[C0 :: C1 :: C2 :: HNil, AA, A, L, LCS0](a)

  implicit class Transfigurable[A](a: A) {
    def transfigureTo[S0[_]] = new TransfigureSugar1[Context.Aux[S0], A](a)
    def transfigureTo[S0[_], S1[_]] = new TransfigureSugar2[Context.Aux[S0], Context.Aux[S1], A](a)
    def transfigureTo[S0[_], S1[_], S2[_]] = new TransfigureSugar3[Context.Aux[S0], Context.Aux[S1], Context.Aux[S2], A](a)

    def mapWith[S0[_]](implicit sh: StackHelper[Context.Aux[S0] :: HNil, A]) =
      new MapSugar1[Context.Aux[S0], A, sh.A, sh.S, sh.CS](a)(sh)

    def mapWith[S0[_], S1[_]](implicit sh: StackHelper[Context.Aux[S0] :: Context.Aux[S1] :: HNil, A]) =
      new MapSugar2[Context.Aux[S0], Context.Aux[S1], A, sh.A, sh.S, sh.CS](a)(sh)

    def mapWith[S0[_], S1[_], S2[_]](implicit sh: StackHelper[Context.Aux[S0] :: Context.Aux[S1] :: Context.Aux[S2] :: HNil, A]) =
      new MapSugar3[Context.Aux[S0], Context.Aux[S1], Context.Aux[S2], A, sh.A, sh.S, sh.CS](a)(sh)
  }
}