package scalaz

import scala.annotation._
import scalaz.Id._

trait Transfigure[F[_], G[_], Z[_]] {
  def transfigure[A, B](fa: F[A])(f: A => Z[B]): G[B]
}

trait TransfigureInstances {

  implicit def join[F[_]]
    (implicit F: Bind[F]) = new Transfigure[λ[α => F[F[α]]], F, Id] {
      def transfigure[A, B](ffa: F[F[A]])(f: A => B): F[B] = F.bind(ffa)(fa => F.map(fa)(f))
    }

  implicit def map[F[_]]
    (implicit F: Functor[F]) = new Transfigure[F, F, Id] {
      def transfigure[A, B](fa: F[A])(f: A => Id[B]): F[B] = F.map(fa)(f)
    }

  implicit def bind[F[_]]
    (implicit F: Bind[F]) = new Transfigure[F, F, F] {
      def transfigure[A, B](fa: F[A])(f: A => F[B]): F[B] = F.bind(fa)(f)
    }

  implicit def traverse[F[_], Z[_]]
    (implicit F: Traverse[F], Z: Applicative[Z]) = new Transfigure[F, λ[α => Z[F[α]]], Z] {
      def transfigure[A, B](fa: F[A])(f: A => Z[B]): Z[F[B]] = F.traverse(fa)(f)
    }

  implicit def traverse_join[F[_], G[_]]
    (implicit F: Monad[F], G: Traverse[G] with Bind[G]) = new Transfigure[λ[α => F[G[α]]], λ[α => F[G[α]]], λ[α => F[G[α]]]] {
      def transfigure[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = F.map(F.bind(fga)(G.traverse(_)(f)))(G.join(_))
    }

  implicit def point[X[_], F[_], G[_], Z[_]]
    (implicit X: Applicative[X], tf: Transfigure[F, G, Z]) = new Transfigure[F, λ[α => X[G[α]]], Z] {
      def transfigure[A, B](fa: F[A])(f: A => Z[B]): X[G[B]] = X.point(tf.transfigure(fa)(f))
    }

    // TODO Find a way to avoid those instances by using recursive implicit resolution
    /*
  implicit def mapR0[X[_], F[_], G[_], Z[_]]
    (implicit X: Functor[X], tf: Transfigure[F, G, Z]) = new Transfigure[λ[α => X[F[α]]], λ[α => X[G[α]]], Z] {
      def transfigure[A, B](xa: X[F[A]])(f: A => Z[B]): X[G[B]] = X.map(xa)(fa => tf.transfigure(fa)(f))
    }

  implicit def mapR1[X0[_], X1[_], F[_], G[_], Z[_]]
    (implicit X0: Functor[X0], X1: Functor[X1], tf: Transfigure[F, G, Z]) = new Transfigure[λ[α => X0[X1[F[α]]]], λ[α => X0[X1[G[α]]]], Z] {
      def transfigure[A, B](x0a: X0[X1[F[A]]])(f: A => Z[B]): X0[X1[G[B]]] = X0.map(x0a)(x1a => X1.map(x1a)(fa => tf.transfigure(fa)(f)))
    }

  implicit def mapR2[X0[_], X1[_], X2[_], F[_], G[_], Z[_]]
    (implicit X0: Functor[X0], X1: Functor[X1], X2: Functor[X2], tf: Transfigure[F, G, Z]) = new Transfigure[λ[α => X0[X1[X2[F[α]]]]], λ[α => X0[X1[X2[G[α]]]]], Z] {
      def transfigure[A, B](x0a: X0[X1[X2[F[A]]]])(f: A => Z[B]): X0[X1[X2[G[B]]]] = X0.map(x0a)(x1a => X1.map(x1a)(x2a => X2.map(x2a)(fa => tf.transfigure(fa)(f))))
    }

  implicit def mapR3[X0[_], X1[_], X2[_], X3[_], F[_], G[_], Z[_]]
    (implicit X0: Functor[X0], X1: Functor[X1], X2: Functor[X2], X3: Functor[X3], tf: Transfigure[F, G, Z]) = new Transfigure[λ[α => X0[X1[X2[X3[F[α]]]]]], λ[α => X0[X1[X2[X3[G[α]]]]]], Z] {
      def transfigure[A, B](x0a: X0[X1[X2[X3[F[A]]]]])(f: A => Z[B]): X0[X1[X2[X3[G[B]]]]] = X0.map(x0a)(x1a => X1.map(x1a)(x2a => X2.map(x2a)(x3a => X3.map(x3a)(fa => tf.transfigure(fa)(f)))))
    }
    */
}

trait TransfigureSyntax {

  implicit class Transfigurable[A](a: A) {
    def transfigureTo[S0[_]] = new TransfigureToS0[A, S0](a)
    def transfigureTo[S0[_], S1[_]] = new TransfigureToS1[A, S0, S1](a)
  }

  class TransfigureToS0[A, S0[_]](a: A) {
    def apply[F](f: F)(implicit U: UnapplyS0[S0, A, F]): U.B = U(a)(f)
  }

  class TransfigureToS1[A, S0[_], S1[_]](a: A) {
    def apply[F](f: F)(implicit U: UnapplyS1[S0, S1, A, F]): U.B = U(a)(f)
  }

  @implicitNotFound(
  """S0
  It's not possible to go from ${A} to ${S0} using ${F}.
  """)
  trait UnapplyS0[S0[_], A, F] {
    type B
    def apply(a: A)(f: F): B
  }

  trait UnapplyS0I0 {
    def fromFunction[S0[_], A, F, BB](x: A => F => BB) = new UnapplyS0[S0, A, F] {
      type B = BB
      def apply(a: A)(f: F): B = x(a)(f)
    }

    implicit def join[S0[_], A, B](implicit ts: Transfigure[λ[α => S0[S0[α]]], S0, Id]): UnapplyS0[S0, S0[S0[A]], A => B] =
      fromFunction[S0, S0[S0[A]], A => B, S0[B]](ts.transfigure)
  }

  object UnapplyS0 extends UnapplyS0I0 {

    implicit def map[S0[_], A, B](implicit ts: Transfigure[S0, S0, Id]): UnapplyS0[S0, S0[A], A => B] =
      fromFunction[S0, S0[A], A => B, S0[B]](ts.transfigure)

    implicit def flatMap[S0[_], A, B](implicit ts: Transfigure[S0, S0, S0]): UnapplyS0[S0, S0[A], A => S0[B]] =
      fromFunction[S0, S0[A], A => S0[B], S0[B]](ts.transfigure)
  }

  @implicitNotFound(
  """S1
  It's not possible to go from ${A} to ${S0}[${S1}] using ${F}.
  """)
  trait UnapplyS1[S0[_], S1[_], A, F] {
    type B
    def apply(a: A)(f: F): B
  }

  object UnapplyS1 {
    def fromFunction[S0[_], S1[_], A, F, BB](x: A => F => BB) = new UnapplyS1[S0, S1, A, F] {
      type B = BB
      def apply(a: A)(f: F): B = x(a)(f)
    }

    implicit def traverse[S0[_], S1[_], A, B](implicit ts: Transfigure[S1, λ[α => S0[S1[α]]], S0]): UnapplyS1[S0, S1, S1[A], A => S0[B]] =
      fromFunction[S0, S1, S1[A], A => S0[B], S0[S1[B]]](ts.transfigure)
  }
}

object Transfigure extends TransfigureInstances with TransfigureSyntax {
  def apply[F[_], G[_], Z[_]](implicit tf: Transfigure[F, G, Z]): Transfigure[F, G, Z] = tf
}
