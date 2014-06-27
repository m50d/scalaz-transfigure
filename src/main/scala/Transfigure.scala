package scalaz

import scalaz.Id._

trait Transfigure[F[_], G[_], Z[_]] {
  def transfigure[A, B](fa: F[A])(f: A => Z[B]): G[B]
}

trait TransfigureInstances {
  implicit def point[X[_], F[_], G[_], Z[_]]
    (implicit X: Applicative[X], tf: Transfigure[F, G, Z]) = new Transfigure[F, λ[α => X[G[α]]], Z] {
      def transfigure[A, B](fa: F[A])(f: A => Z[B]): X[G[B]] = X.point(tf.transfigure(fa)(f))
    }

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

  implicit def mapR[X[_], F[_], G[_], Z[_]]
    (implicit X: Functor[X], tf: Transfigure[F, G, Z]) = new Transfigure[λ[α => X[F[α]]], λ[α => X[G[α]]], Z] {
      def transfigure[A, B](xa: X[F[A]])(f: A => Z[B]): X[G[B]] = X.map(xa)(fa => tf.transfigure(fa)(f))
    }
}

object Transfigure extends TransfigureInstances {
  def apply[F[_], G[_], Z[_]](implicit tf: Transfigure[F, G, Z]): Transfigure[F, G, Z] = tf
}
