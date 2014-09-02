package scalaz.transfigure

import shapeless._
import ops.hlist.Length
import scalaz._
import scalaz.Scalaz._
import Nat._0

trait LT[A <: Nat, B <: Nat]

object LT {
  implicit def lt1[B <: Nat] = new LT[_0, Succ[B]] {}
  implicit def lt2[A <: Nat, B <: Nat](implicit lt: LT[A, B]) =
    new LT[Succ[A], Succ[B]] {}
}

trait GT[A <: Nat, B <: Nat]

object GT {
  implicit def gt1[B <: Nat] = new GT[Succ[B], _0] {}
  implicit def gt2[A <: Nat, B <: Nat](implicit gt: GT[A, B]) =
    new GT[Succ[A], Succ[B]] {}
}

trait IndexOf[Idx <: HList, A] {
  type Out <: Nat
}

trait LowPriorityIndexOf {
  implicit def cons[A, B, Remainder <: HList](implicit i: IndexOf[Remainder, A]) = new IndexOf[B :: Remainder, A] {
    type Out = i.Out
  }
}

object IndexOf extends LowPriorityIndexOf {
  implicit def head[A, Remainder <: HList](implicit length: Length[Remainder]) = new IndexOf[A :: Remainder, A] {
    type Out = length.Out
  }

  type Aux[Idx <: HList, A, N <: Nat] = IndexOf[Idx, A] { type Out = N }
  def apply[Idx <: HList, A](implicit io: IndexOf[Idx, A]): Aux[Idx, A, io.Out] = io
}

trait IdxAndLtEq[Idx <: HList, A, B] {
  type Out <: LT[_ <: Nat, _ <: Nat]
}

object IdxAndLtEq {
  implicit def byIndex[Idx <: HList, A, B](
    implicit i1: IndexOf[Idx, A], i2: IndexOf[Idx, B]) = new IdxAndLtEq[Idx, A, B] {
    type Out = LT[i1.Out, i2.Out]
  }
  type Aux[Idx <: HList, A, B, O <: LT[_ <: Nat, _ <: Nat]] = IdxAndLtEq[Idx, A, B] { type Out = O }
  def apply[Idx <: HList, A, B](implicit ile: IdxAndLtEq[Idx, A, B]): Aux[Idx, A, B, ile.Out] = ile
}

trait IdxAndGt[Idx <: HList, A, B] {
  type Out <: GT[_ <: Nat, _ <: Nat]
}

object IdxAndGt {
  implicit def byIndex[Idx <: HList, A, B](
    implicit i1: IndexOf[Idx, A], i2: IndexOf[Idx, B]) = new IdxAndGt[Idx, A, B] {
    type Out = GT[i1.Out, i2.Out]
  }
  type Aux[Idx <: HList, A, B, O <: GT[_ <: Nat, _ <: Nat]] = IdxAndGt[Idx, A, B] { type Out = O }
}

trait LTIndexed[Idx <: HList, A, B]
object LTIndexed {
  implicit def ltEqIndexed[Idx <: HList, A, B, O <: LT[_ <: Nat, _ <: Nat]](implicit i: IdxAndLtEq.Aux[Idx, A, B, O], l: O) =
    new LTIndexed[Idx, A, B] {}
}

trait GTIndexed[Idx <: HList, A, B]
object GTIndexed {
  implicit def gtIndexed[Idx <: HList, A, B, O <: GT[_ <: Nat, _ <: Nat]](implicit i: IdxAndGt.Aux[Idx, A, B, O], l: O) =
    new GTIndexed[Idx, A, B] {}
}

trait NonDecreasingIndexed[Idx <: HList, L <: HList]

object NonDecreasingIndexed {
  implicit def hnilNonDecreasing[Idx <: HList] = new NonDecreasingIndexed[Idx, HNil] {}
  implicit def hlistNonDecreasing1[Idx <: HList, H] = new NonDecreasingIndexed[Idx, H :: HNil] {}
  implicit def hlistNonDecreasing2[Idx <: HList, H1, H2, T <: HList](implicit ltEq: LTIndexed[Idx, H1, H2], ndt: NonDecreasingIndexed[Idx, H2 :: T]) =
    new NonDecreasingIndexed[Idx, H1 :: H2 :: T] {}
}

trait Context {
  type C[_]
}

object Context {
  type Aux[N[_]] = Context {
    type C[A] = N[A]
  }
}

trait ContextStack[L <: HList] {
  type Out[_]
}

trait SelectionStep[Idx <: HList, C <: Context, D <: Context] {
  type X <: Context
  type Y <: Context

  type I[A] = C#C[D#C[A]]
  type O[A] = X#C[Y#C[A]]

  val trans: NaturalTransformation[I, O]
}

object SelectionStep {
  implicit def lt[Idx <: HList, C <: Context, D <: Context](implicit lt: LTIndexed[Idx, C, D]) = new SelectionStep[Idx, C, D] {
    type X = C
    type Y = D
    val trans = new NaturalTransformation[I, O] {
      def apply[A](fa: C#C[D#C[A]]) = fa
    }
  }

  implicit def gt[Idx <: HList, C <: Context, D <: Context](implicit gt: GTIndexed[Idx, C, D], traverse: Traverse[C#C], ap: Applicative[D#C]) = new SelectionStep[Idx, C, D] {
    type X = D
    type Y = C
    val trans = new NaturalTransformation[I, O] {
      def apply[A](fa: C#C[D#C[A]]) = fa.sequence
    }
  }

  type Aux[Idx <: HList, C <: Context, D <: Context, X1 <: Context, Y1 <: Context] = SelectionStep[Idx, C, D] {
    type X = X1
    type Y = Y1
  }
  def apply[Idx <: HList, C <: Context, D <: Context](implicit ss: SelectionStep[Idx, C, D]): Aux[Idx, C, D, ss.X, ss.Y] =
    ss
}

trait SelectLeast[Idx <: HList, L <: HList] {
  type C <: Context
  type R <: HList
  //  type LCS <: ContextStack[L]
  //  type RCS <: ContextStack[R]
  //  type CRCS[A] = C#C[RCS#Out[A]]
  //
  //  val trans: NaturalTransformation[LCS#Out, CRCS]
}

object SelectLeast {
  type Aux[Idx <: HList, L <: HList, C1 <: Context, R1 <: HList] = SelectLeast[Idx, L] {
    type C = C1
    type R = R1
  }

  implicit def selectLeast1[Idx <: HList, C1 <: Context] = new SelectLeast[Idx, C1 :: HNil] {
    type C = C1
    type R = HNil
    //    type LCS = ContextStack[C :: HNil] {
    //      type Out[A] = C#C[A]
    //    }
    //    type RCS = ContextStack[HNil] {
    //      type Out[A] = A
    //    }
    //
    //    val trans = new NaturalTransformation[C#C, C#C] {
    //      def apply[A](fa: C#C[A]) = fa
    //    }
  }

  implicit def selectLeastCons[Idx <: HList, C <: Context, D <: Context, R1 <: HList](
    implicit step: SelectionStep[Idx, C, D], tl: SelectLeast[Idx, D :: R1]) =
    new SelectLeast[Idx, C :: D :: R1] {
      type C = step.X
      type R = step.Y :: tl.R
    }

  //  def selectLeast[Idx <: HList, L <: HList](idx: Idx, l: L)(implicit sl: SelectLeast[Idx, L]): NaturalTransformation[sl.LCS#Out, sl.CRCS] =
  //    sl.trans
}

trait SelectionSort[Idx <: HList, I <: HList, O <: HList] {
  type ICS <: ContextStack[I]
  type OCS <: ContextStack[O]

  val trans: NaturalTransformation[ICS#Out, OCS#Out]
}

trait LowPrioritySelectionSort {}

object SelectionSort extends LowPrioritySelectionSort {

}

trait Transfigure[F[_], G[_], Z[_]] {
  def transfigure[A, B](fa: F[A])(f: A ⇒ Z[B]): G[B]
}

trait TransfigureInstances {
  //  implicit object ZERO extends Transfigure[Id, Id, Id] {
  //    def transfigure[A, B](fa: A)(f: A ⇒ B): B = f(fa)
  //  }

  //  implicit def point[X[_], F[_], G[_], Z[_]](implicit X: Applicative[X], tf: Transfigure[F, G, Z]) = new Transfigure[F, λ[α ⇒ X[G[α]]], Z] {
  //    def transfigure[A, B](fa: F[A])(f: A ⇒ Z[B]): X[G[B]] = X.point(tf.transfigure(fa)(f))
  //  }

  //  implicit def bottomMapR[X[_], F[_], G[_], Z[_]](implicit tf: Transfigure[F, G, Z]) = new Transfigure[F, λ[α ⇒ G[X[α]]], λ[α ⇒ Z[X[α]]]] {
  //    def transfigure[A, B](fa: F[A])(f: A ⇒ Z[X[B]]): G[X[B]] = tf.transfigure(fa)(f)
  //  }
  //
  //  implicit def topMapL[X[_], F[_], G[_], Z[_]](implicit X: Functor[X], tf: Transfigure[F, G, Z]) = new Transfigure[λ[α ⇒ X[F[α]]], λ[α ⇒ X[G[α]]], Z] {
  //    def transfigure[A, B](fa: X[F[A]])(f: A ⇒ Z[B]): X[G[B]] = fa map { tf.transfigure(_)(f) }
  //  }

  //  implicit def bindL[X[_], F[_], G[_], Z[_]]

  //
  //  implicit def join[F[_]](implicit F: Bind[F]) = new Transfigure[λ[α ⇒ F[F[α]]], F, Id] {
  //    def transfigure[A, B](ffa: F[F[A]])(f: A ⇒ B): F[B] = F.bind(ffa)(fa ⇒ F.map(fa)(f))
  //  }
  //
  //  implicit def map[F[_]](implicit F: Functor[F]) = new Transfigure[F, F, Id] {
  //    def transfigure[A, B](fa: F[A])(f: A ⇒ Id[B]): F[B] = F.map(fa)(f)
  //  }
  //
  //  implicit def bind[F[_]](implicit F: Bind[F]) = new Transfigure[F, F, F] {
  //    def transfigure[A, B](fa: F[A])(f: A ⇒ F[B]): F[B] = F.bind(fa)(f)
  //  }
  //
  //  implicit def bind_traverse[F[_], G[_]](implicit F: Bind[F] with Applicative[F], G: Traverse[G]) = new Transfigure[λ[α ⇒ F[G[α]]], λ[α ⇒ F[G[α]]], F] {
  //    def transfigure[A, B](fga: F[G[A]])(f: A ⇒ F[B]): F[G[B]] = F.bind(fga)(ga ⇒ G.traverse(ga)(f))
  //  }
  //
  //  implicit def traverse[F[_], Z[_]](implicit F: Traverse[F], Z: Applicative[Z]) = new Transfigure[F, λ[α ⇒ Z[F[α]]], Z] {
  //    def transfigure[A, B](fa: F[A])(f: A ⇒ Z[B]): Z[F[B]] = F.traverse(fa)(f)
  //  }
  //
  //  implicit def traverse_join[F[_], G[_]](implicit F: Monad[F], G: Traverse[G] with Bind[G]) = new Transfigure[λ[α ⇒ F[G[α]]], λ[α ⇒ F[G[α]]], λ[α ⇒ F[G[α]]]] {
  //    def transfigure[A, B](fga: F[G[A]])(f: A ⇒ F[G[B]]): F[G[B]] = F.map(F.bind(fga)(G.traverse(_)(f)))(G.join(_))
  //  }
  //
  //  implicit def mapR[X[_], F[_], G[_], Z[_]](implicit X: Functor[X], tf: Transfigure[F, G, Z]) = new Transfigure[λ[α ⇒ X[F[α]]], λ[α ⇒ X[G[α]]], Z] {
  //    def transfigure[A, B](xa: X[F[A]])(f: A ⇒ Z[B]): X[G[B]] = X.map(xa)(fa ⇒ tf.transfigure(fa)(f))
  //  }
}

object Transfigure extends TransfigureInstances {
  def apply[F[_], G[_], Z[_]](implicit tf: Transfigure[F, G, Z]): Transfigure[F, G, Z] = tf
}
