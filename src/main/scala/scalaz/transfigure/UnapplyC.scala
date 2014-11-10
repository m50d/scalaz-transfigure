package scalaz.transfigure
import scalaz.Leibniz._
import scala.language.higherKinds

trait UnapplyC[MA] {

  type M <: Context

  /** The type that `M` was applied to */
  type A

  /** Evidence that MA =:= M[A] */
  def leibniz: MA === M#C[A]
}

/**
 * TODO: copy more implementations from scalaz.Unapply so that we can unapply e.g. Either, State.
 */
object UnapplyC {

  implicit def unapplyMA[M0[_], A0]: UnapplyC[M0[A0]] {
    type M = Context.Aux[M0]
    type A = A0
  } = new UnapplyC[M0[A0]] {
    type M = Context.Aux[M0]
    type A = A0
    def leibniz = refl
  }
}