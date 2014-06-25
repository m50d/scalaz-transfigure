package scalaz

import scalaz.Id._

trait Transfigure[F[_], G[_], Z[_]] {
  def transfigure[A, B](fa: F[A])(f: A => Z[B]): G[B]
}

trait TransfigureInstances {

  implicit def map[F[_]]
    (implicit F: Functor[F]) = new Transfigure[F, F, Id] {
      def transfigure[A, B](fa: F[A])(f: A => Id[B]): F[B] = F.map(fa)(f)
    }

  implicit def bind[F[_]]
    (implicit F: Bind[F]) = new Transfigure[F, F, F] {
      def transfigure[A, B](fa: F[A])(f: A => F[B]): F[B] = F.bind(fa)(f)
    }

  implicit def traverse[F[_], Z[_]]
    (implicit F: Traverse[F], Z: Applicative[Z]) = new Transfigure[F, ({type λ[α] = Z[F[α]]})#λ, Z] {
      def transfigure[A, B](fa: F[A])(f: A => Z[B]): Z[F[B]] = F.traverse(fa)(f)
    }

  implicit def mapR0[X[_], F[_], G[_], Z[_]]
    (implicit X: Functor[X], tf: Transfigure[F, G, Z]) = new Transfigure[({type λ[α] = X[F[α]]})#λ, ({type λ[α] = X[G[α]]})#λ, Z] {
      def transfigure[A, B](xa: X[F[A]])(f: A => Z[B]): X[G[B]] = X.map(xa)(fa => tf.transfigure(fa)(f))
    }

  implicit def mapR1[X0[_], X1[_], F[_], G[_], Z[_]]
    (implicit X0: Functor[X0], X1: Functor[X1], tf: Transfigure[F, G, Z]) = new Transfigure[({type λ[α] = X0[X1[F[α]]]})#λ, ({type λ[α] =X0[X1[G[α]]]})#λ, Z] {
      def transfigure[A, B](x0a: X0[X1[F[A]]])(f: A => Z[B]): X0[X1[G[B]]] = X0.map(x0a)(x1a => X1.map(x1a)(fa => tf.transfigure(fa)(f)))
    }
}

trait TransfigureSyntax {
  implicit class Transfigurable0[F0[_], A](fa: F0[A]) {
    def transfigureTo[G[_]] = new TransfigureTo[F0, G, A](fa)
  }

  implicit class Transfigurable1[F0[_], F1[_], A](fa: F0[F1[A]]) {
    def transfigureTo[G[_]] = new TransfigureTo[({type λ[α] = F0[F1[α]]})#λ, G, A](fa)
  }

  implicit class Transfigurable2[F0[_], F1[_], F2[_], A](fa: F0[F1[F2[A]]]) {
    def transfigureTo[G[_]] = new TransfigureTo[({type λ[α] = F0[F1[F2[α]]]})#λ, G, A](fa)
  }

  implicit class Transfigurable3[F0[_], F1[_], F2[_], F3[_], A](fa: F0[F1[F2[F3[A]]]]) {
    def transfigureTo[G[_]] = new TransfigureTo[({type λ[α] = F0[F1[F2[F3[α]]]]})#λ, G, A](fa)
  }

  class TransfigureTo[F[_], G[_], A](fa: F[A]) {
    def apply[Z[_], B](f: A => Z[B])(implicit tf: Transfigure[F, G, Z]): G[B] = tf.transfigure(fa)(f)
  }
}

object Transfigure extends TransfigureInstances with TransfigureSyntax {
  def apply[F[_], G[_], Z[_]](implicit tf: Transfigure[F, G, Z]): Transfigure[F, G, Z] = tf
}
