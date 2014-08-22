package scalaz

import scala.annotation._
import scalaz.Id._
import scala.language.experimental.macros
import scala.reflect.macros.Context

trait TransfigureToSyntax {
  implicit class Transfigurable[A](a: A) {
    def transfigureTo[S0[_]] = new TransfigureToS0[A, S0](a)
    def transfigureTo[S0[_], S1[_]] = new TransfigureToS1[A, S0, S1](a)
    def transfigureTo[S0[_], S1[_], S2[_]] = new TransfigureToS2[A, S0, S1, S2](a)
  }
}

class TransfigureToS0[A, S0[_]](a: A) {
  def apply[F, B](f: F)(implicit U: TransfigureTo1.UnapplyS0[S0, A, F, B]): B = U(a)(f)
}

class TransfigureToS1[A, S0[_], S1[_]](a: A) {
  def apply[F, B](f: F)(implicit U: TransfigureTo.UnapplyS1[S0, S1, A, F, B]): B = U(a)(f)
}

class TransfigureToS2[A, S0[_], S1[_], S2[_]](a: A) {
  def apply[F, B](f: F)(implicit U: TransfigureTo.UnapplyS2[S0, S1, S2, A, F, B]): B = U(a)(f)
}

object TransfigureTo {
  val syntax = new TransfigureToSyntax {}

  @implicitNotFound(
    """S1
  It's not possible to go from ${A} to ${S0}[${S1}] using ${F}.
  """)
  trait UnapplyS1[S0[_], S1[_], A, F, B] {
    def apply(a: A)(f: F): B
  }

  trait UnapplyS1I0 {
    def fromFunction[S0[_], S1[_], A, F, B](x: A ⇒ F ⇒ B) = new UnapplyS1[S0, S1, A, F, B] {
      def apply(a: A)(f: F): B = x(a)(f)
    }

    implicit def point[S0[_], S1[_], A, B](implicit ts: Transfigure[S1, λ[α ⇒ S0[S1[α]]], Id]): UnapplyS1[S0, S1, S1[A], A ⇒ B, S0[S1[B]]] =
      fromFunction(ts.transfigure)

  }

  trait UnapplyS1I1 extends UnapplyS1I0 {
    implicit def traverse[S0[_], S1[_], A, B](implicit ts: Transfigure[S1, λ[α ⇒ S0[S1[α]]], S0]): UnapplyS1[S0, S1, S1[A], A ⇒ S0[B], S0[S1[B]]] =
      fromFunction(ts.transfigure)

    implicit def map_map[S0[_], S1[_], A, B](implicit ts: Transfigure[λ[α ⇒ S0[S1[α]]], λ[α ⇒ S0[S1[α]]], Id]): UnapplyS1[S0, S1, S0[S1[A]], A ⇒ B, S0[S1[B]]] =
      fromFunction(ts.transfigure)
  }

  trait UnapplyS1I2 extends UnapplyS1I1 {
    implicit def map_flatMap[S0[_], S1[_], A, B](implicit ts: Transfigure[λ[α ⇒ S0[S1[α]]], λ[α ⇒ S0[S1[α]]], S1]): UnapplyS1[S0, S1, S0[S1[A]], A ⇒ S1[B], S0[S1[B]]] =
      fromFunction(ts.transfigure)

    implicit def bind_traverse[S0[_], S1[_], A, B](implicit ts: Transfigure[λ[α ⇒ S0[S1[α]]], λ[α ⇒ S0[S1[α]]], S0]): UnapplyS1[S0, S1, S0[S1[A]], A ⇒ S0[B], S0[S1[B]]] =
      fromFunction(ts.transfigure)
  }

  object UnapplyS1 extends UnapplyS1I2 {
    implicit def traverse_join[S0[_], S1[_], A, B](implicit ts: Transfigure[λ[α ⇒ S0[S1[α]]], λ[α ⇒ S0[S1[α]]], λ[α ⇒ S0[S1[α]]]]): UnapplyS1[S0, S1, S0[S1[A]], A ⇒ S0[S1[B]], S0[S1[B]]] =
      fromFunction(ts.transfigure)

  }

  @implicitNotFound(
    """S2
  It's not possible to go from ${A} to ${S0}[${S1}[${S2}]] using ${F}.
  """)
  trait UnapplyS2[S0[_], S1[_], S2[_], A, F, B] {
    def apply(a: A)(f: F): B
  }
}

@TransfigureToMacro
object TransfigureTo1 {
  @implicitNotFound(
    """S0
  It's not possible to go from ${A} to ${S0} using ${F}.
  """)
  trait UnapplyS0[S0[_], A, F, B] {
    def apply(a: A)(f: F): B
  }
}
