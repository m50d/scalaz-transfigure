package scalaz

import scala.annotation._
import scalaz.Id._

trait TransfigureToSyntax {
  implicit class Transfigurable[A](a: A) {
    def transfigureTo[S0[_]] = new TransfigureToS0[A, S0](a)
    def transfigureTo[S0[_], S1[_]] = new TransfigureToS1[A, S0, S1](a)
  }
}

class TransfigureToS0[A, S0[_]](a: A) {
  def apply[F](f: F)(implicit U: TransfigureTo.UnapplyS0[S0, A, F]): U.B = U(a)(f)
}

class TransfigureToS1[A, S0[_], S1[_]](a: A) {
  def apply[F](f: F)(implicit U: TransfigureTo.UnapplyS1[S0, S1, A, F]): U.B = U(a)(f)
}

object TransfigureTo {
  val syntax = new TransfigureToSyntax {}

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
