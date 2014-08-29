package scalaz.transfigure

import shapeless._
import ops.hlist.Length

trait LTEq[A <: Nat, B <: Nat]

object LTEq {
  import Nat._0

  type <=[A <: Nat, B <: Nat] = LTEq[A, B]

  implicit def ltEq1 = new <=[_0, _0] {}
  implicit def ltEq2[B <: Nat] = new <=[_0, Succ[B]] {}
  implicit def ltEq3[A <: Nat, B <: Nat](implicit lt: A <= B) =
    new <=[Succ[A], Succ[B]] {}
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
  type Out <: LTEq[_ <: Nat, _ <: Nat]
}

object IdxAndLtEq {
  implicit def byIndex[Idx <: HList, A, B](
    implicit i1: IndexOf[Idx, A], i2: IndexOf[Idx, B]) = new IdxAndLtEq[Idx, A, B] {
    type Out = LTEq[i1.Out, i2.Out]
  }
  type Aux[Idx <: HList, A, B, O <: LTEq[_ <: Nat, _ <: Nat]] = IdxAndLtEq[Idx, A, B] { type Out = O }
  def apply[Idx <: HList, A, B](implicit ile: IdxAndLtEq[Idx, A, B]): Aux[Idx, A, B, ile.Out] = ile
}

trait LTEqIndexed[Idx <: HList, A, B]
object LTEqIndexed {
  implicit def ltEqIndexed[Idx <: HList, A, B](implicit i: IdxAndLtEq[Idx, A, B], l: i.Out) =
    new LTEqIndexed[Idx, A, B] {}
}

//  trait NonDecreasing[L <: HList]
//  implicit def hnilNonDecreasing = new NonDecreasing[HNil] {}
//  implicit def hlistNonDecreasing1[H] = new NonDecreasing[H :: HNil] {}
//  implicit def hlistNonDecreasing2[H1 <: Nat, H2 <: Nat, T <: HList]
//    (implicit ltEq : H1 <= H2, ndt : NonDecreasing[H2 :: T]) = new NonDecreasing[H1 :: H2 :: T] {}

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
