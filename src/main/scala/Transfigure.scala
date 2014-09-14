package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import ops.hlist.Length
import scalaz._
import scalaz.Scalaz._
import Nat._0

sealed trait LE[A <: Nat, B <: Nat]

object LE {
  implicit def le1[B <: Nat] = new LE[_0, B] {}
  implicit def le2[A <: Nat, B <: Nat](implicit le: LE[A, B]) =
    new LE[Succ[A], Succ[B]] {}
}

sealed trait GT[A <: Nat, B <: Nat]

object GT {
  implicit def gt1[B <: Nat] = new GT[Succ[B], _0] {}
  implicit def gt2[A <: Nat, B <: Nat](implicit gt: GT[A, B]) =
    new GT[Succ[A], Succ[B]] {}
}

sealed trait IndexOf[Idx <: HList, A] {
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

sealed trait IdxAndLtEq[Idx <: HList, A, B] {
  type Out <: LE[_ <: Nat, _ <: Nat]
}

object IdxAndLtEq {
  implicit def byIndex[Idx <: HList, A, B](
    implicit i1: IndexOf[Idx, A], i2: IndexOf[Idx, B]) = new IdxAndLtEq[Idx, A, B] {
    type Out = LE[i1.Out, i2.Out]
  }
  type Aux[Idx <: HList, A, B, O <: LE[_ <: Nat, _ <: Nat]] = IdxAndLtEq[Idx, A, B] { type Out = O }
  def apply[Idx <: HList, A, B](implicit ile: IdxAndLtEq[Idx, A, B]): Aux[Idx, A, B, ile.Out] = ile
}

sealed trait IdxAndGt[Idx <: HList, A, B] {
  type Out <: GT[_ <: Nat, _ <: Nat]
}

object IdxAndGt {
  implicit def byIndex[Idx <: HList, A, B](
    implicit i1: IndexOf[Idx, A], i2: IndexOf[Idx, B]) = new IdxAndGt[Idx, A, B] {
    type Out = GT[i1.Out, i2.Out]
  }
  type Aux[Idx <: HList, A, B, O <: GT[_ <: Nat, _ <: Nat]] = IdxAndGt[Idx, A, B] { type Out = O }
}

sealed trait LEIndexed[Idx <: HList, A, B]
object LEIndexed {
  implicit def ltEqIndexed[Idx <: HList, A, B, O <: LE[_ <: Nat, _ <: Nat]](implicit i: IdxAndLtEq.Aux[Idx, A, B, O], l: O) =
    new LEIndexed[Idx, A, B] {}
}

sealed trait GTIndexed[Idx <: HList, A, B]
object GTIndexed {
  implicit def gtIndexed[Idx <: HList, A, B, O <: GT[_ <: Nat, _ <: Nat]](implicit i: IdxAndGt.Aux[Idx, A, B, O], l: O) =
    new GTIndexed[Idx, A, B] {}
}

sealed trait NonDecreasingIndexed[Idx <: HList, L <: HList]

object NonDecreasingIndexed {
  implicit def hnilNonDecreasing[Idx <: HList] = new NonDecreasingIndexed[Idx, HNil] {}
  implicit def hlistNonDecreasing1[Idx <: HList, H] = new NonDecreasingIndexed[Idx, H :: HNil] {}
  implicit def hlistNonDecreasing2[Idx <: HList, H1, H2, T <: HList](implicit ltEq: LEIndexed[Idx, H1, H2], ndt: NonDecreasingIndexed[Idx, H2 :: T]) =
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

sealed trait SelectionStep[Idx <: HList, C1 <: Context, D <: Context] {
  type X <: Context
  type Y <: Context

  type I = Context {
    type C[A] = C1#C[D#C[A]]
  }
  type O = Context {
    type C[A] = X#C[Y#C[A]]
  }

  val trans: NaturalTransformation[I#C, O#C]
}

object SelectionStep {
  implicit def gt[Idx <: HList, C <: Context, D <: Context](implicit gt: GTIndexed[Idx, C, D]) = new SelectionStep[Idx, C, D] {
    type X = C
    type Y = D
    val trans = new NaturalTransformation[I#C, O#C] {
      def apply[A](fa: C#C[D#C[A]]) = fa
    }
  }

  implicit def le[Idx <: HList, C <: Context, D <: Context](implicit le: LEIndexed[Idx, C, D], traverse: Traverse[C#C], ap: Applicative[D#C]) = new SelectionStep[Idx, C, D] {
    type X = D
    type Y = C
    val trans = new NaturalTransformation[I#C, O#C] {
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

sealed trait SelectLeast[Idx <: HList, L <: HList] {
  type X <: Context
  type R <: HList

  type LCS <: Context
  type RCS <: Context

  val trans: NaturalTransformation[LCS#C, ({ type CRCS[A] = X#C[RCS#C[A]] })#CRCS]
}

object SelectLeast {
  type Aux[Idx <: HList, L <: HList, C <: Context, R1 <: HList] = SelectLeast[Idx, L] {
    type X = C
    type R = R1
  }

  implicit def selectLeast1[Idx <: HList, C <: Context] = new SelectLeast[Idx, C :: HNil] {
    type X = C
    type R = HNil
    type LCS = C
    type RCS = Context.Aux[Id]
    val trans = new NaturalTransformation[LCS#C, ({ type CRCS[A] = X#C[RCS#C[A]] })#CRCS] {
      def apply[A](fa: C#C[A]) = fa
    }
  }

  implicit def selectLeastCons[Idx <: HList, L <: HList, C1 <: Context, D <: Context](
    implicit tl: SelectLeast[Idx, L] {
      type X = D
    }, step: SelectionStep[Idx, C1, D], f: Functor[C1#C]) =
    new SelectLeast[Idx, C1 :: L] {
      type X = step.X
      type R = step.Y :: tl.R

      type LCS = Context {
        type C[A] = C1#C[tl.LCS#C[A]]
      }
      type RCS = Context {
        type C[A] = step.Y#C[tl.RCS#C[A]]
      }

      val trans = new NaturalTransformation[LCS#C, ({ type CRCS[A] = X#C[RCS#C[A]] })#CRCS] {
        def apply[A](fa: C1#C[tl.LCS#C[A]]) = {
          val ga: C1#C[tl.X#C[tl.RCS#C[A]]] = fa map {
            tl.trans.apply(_)
          }
          step.trans(ga)
        }
      }
    }

  def selectLeast[Idx <: HList, L <: HList](implicit sl: SelectLeast[Idx, L]): NaturalTransformation[sl.LCS#C, ({ type CRCS[A] = sl.X#C[sl.RCS#C[A]] })#CRCS] =
    sl.trans
}

sealed trait Leib1[C <: Context, D <: Context] {
  def subst[F[_[_]]](fc: F[C#C]): F[D#C]
  def witness[A](c: C#C[A]): D#C[A] = subst[({ type L[C[_]] = C[A] })#L](c)
}

object Leib1 {
  implicit def refl[C <: Context] = new Leib1[C, C] {
    def subst[F[_[_]]](fa: F[C#C]) = fa
  }
}

sealed trait SelectionSort[Idx <: HList, L <: HList] {
  type ICS <: Context
  type O <: HList
  type OCS <: Context

  val trans: NaturalTransformation[ICS#C, OCS#C]
}

object SelectionSort {
  implicit def nil[Idx <: HList] = new SelectionSort[Idx, HNil] {
    type ICS = Context.Aux[Id]
    type O = HNil
    type OCS = Context.Aux[Id]

    val trans = new NaturalTransformation[ICS#C, OCS#C] {
      def apply[A](fa: A) = fa
    }
  }

  implicit def cons[Idx <: HList, L <: HList, C1 <: Context, R <: HList, SLR <: Context, TLI <: Context](implicit sl: SelectLeast.Aux[Idx, L, C1, R] {
    type RCS = SLR
  },
    tl: SelectionSort[Idx, R] {
      type ICS = TLI
    }, f: Functor[C1#C], w: Leib1[SLR, TLI]) =
    new SelectionSort[Idx, L] {
      type ICS = sl.LCS
      type O = C1 :: tl.O
      type OCS = Context {
        type C[A] = C1#C[tl.OCS#C[A]]
      }
      val trans = new NaturalTransformation[ICS#C, OCS#C] {
        def apply[A](ffa: ICS#C[A]) =
          {
            val stepped = sl.trans.apply(ffa)
            stepped.map {
              fa: sl.RCS#C[A] ⇒
                val ga: SLR#C[A] = fa
                val ha: TLI#C[A] = w.subst[({ type L[B[_]] = B[A] })#L](ga)
                val ia: tl.ICS#C[A] = ha
                tl.trans.apply(ia)
            }
          }
      }
    }

  type Aux[Idx <: HList, L <: HList, ICS1 <: Context, O1 <: HList, OCS1 <: Context] = SelectionSort[Idx, L] {
    type ICS = ICS1
    type O = O1
    type OCS = OCS1
  }
  def apply[Idx <: HList, L <: HList](implicit ss: SelectionSort[Idx, L]): Aux[Idx, L, ss.ICS, ss.O, ss.OCS] = ss
  
  def selectionSort[Idx <: HList, L <: HList](implicit ss: SelectionSort[Idx, L]): NaturalTransformation[ss.ICS#C, ss.OCS#C] =
    ss.trans
}

trait Normalizer[Idx <: HList, L <: HList] {
  type ICS <: Context
  type OCS <: Context
  val trans: NaturalTransformation[ICS#C, OCS#C]
}

trait Normalizer3 {
  implicit object realNil extends Normalizer[HNil, HNil] {
    type ICS = Context.Aux[Id]
    type OCS = Context.Aux[Id]

    val trans = new NaturalTransformation[ICS#C, OCS#C] {
      def apply[A](fa: A) = fa
    }
  }

  implicit def nil[H <: Context, T <: HList, L <: HList](implicit rest: Normalizer[T, L], ap: Applicative[H#C]) = new Normalizer[H :: T, L] {
    type ICS = rest.ICS
    type OCS = Context {
      type C[A] = H#C[rest.OCS#C[A]]
    }
    val trans = new NaturalTransformation[ICS#C, OCS#C] {
      def apply[A](fa: ICS#C[A]) =
        Applicative[H#C].point(rest.trans.apply(fa))
    }
  }
}

trait Normalizer2 extends Normalizer3 {
  implicit def one[H <: Context, T <: HList, L <: HList](implicit rest: Normalizer[T, L], f: Functor[H#C]) = new Normalizer[H :: T, H :: L] {
    type ICS = Context {
      type C[A] = H#C[rest.ICS#C[A]]
    }
    type OCS = Context {
      type C[A] = H#C[rest.OCS#C[A]]
    }
    val trans = new NaturalTransformation[ICS#C, OCS#C] {
      def apply[A](fa: ICS#C[A]) =
        fa map { rest.trans.apply(_) }
    }
  }
}

object Normalizer extends Normalizer2 {
  implicit def two[H <: Context, T <: HList, L <: HList, D <: Context](implicit rest: Normalizer[H :: T, H :: L] {
    type OCS = Context {
      type C[A] = H#C[D#C[A]]
    }
  },
    b: Bind[H#C]) = new Normalizer[H :: T, H :: H :: L] {
    type ICS = Context {
      type C[A] = H#C[rest.ICS#C[A]]
    }
    type OCS = Context {
      type C[A] = rest.OCS#C[A]
    }
    val trans = new NaturalTransformation[ICS#C, OCS#C] {
      def apply[A](fa: ICS#C[A]) =
        fa map { rest.trans.apply(_) } μ
    }
  }
}

/**
 * Substitute for MonadTrans. Inspired by Haskell's layers package,
 * but not (yet) as general or elegant.
 */
trait Layer[M[_]] {
  def monad[F[_]: Monad]: Monad[({ type L[A] = F[M[A]] })#L]
}

object Layer {
  implicit object IdLayer extends Layer[Id] {
    def monad[F[_]: Monad] = Monad[F]
  }

  implicit object OptionLayer extends Layer[Option] {
    def monad[F[_]: Monad] = new Monad[({ type L[A] = F[Option[A]] })#L] {
      def point[A](a: ⇒ A) = Monad[F].point(Some(a))
      def bind[A, B](fa: F[Option[A]])(f: A ⇒ F[Option[B]]) =
        Monad[F].bind(fa) {
          case None ⇒ Monad[F].point(None: Option[B])
          case Some(z) ⇒ f(z)
        }
    }
  }

  implicit object ListLayer extends Layer[List] {
    def monad[F[_]: Monad] = new Monad[({ type L[A] = F[List[A]] })#L] {
      def point[A](a: ⇒ A) = Monad[F].point(List(a))
      def bind[A, B](fa: F[List[A]])(f: A ⇒ F[List[B]]) = {
        def ++(as: F[List[B]], bs: F[List[B]]) = Monad[F].bind(as) { list1 ⇒
          Monad[F].map(bs) { list2 ⇒
            list1 ++ list2
          }
        }

        Monad[F].bind(fa) {
          case Nil ⇒ Monad[F].point(Nil)
          case nonEmpty ⇒ nonEmpty.map(f).reduce[F[List[B]]](++ _)
        }
      }
    }
  }
}

trait MonadStack[L <: HList] {
  type CS <: Context

  val m: Monad[CS#C]
}

object MonadStack {
  implicit object nil extends MonadStack[HNil] {
    type CS = Context.Aux[Id]

    val m = Id.id
  }

  implicit def cons[H <: Context, T <: HList, D <: Context](implicit rest: MonadStack[T] {
    type CS = D
  }, l: Layer[D#C], mo: Monad[H#C]) =
    new MonadStack[H :: T] {
      type CS = Context {
        type C[A] = H#C[rest.CS#C[A]]
      }

      val m = l.monad[H#C](mo)
    }
}

trait SuperNaturalTransformation[-F[_], -G[_], +H[_]] {
  def apply[A, B](f: F[A])(g: A ⇒ G[B]): H[B]
}

/**
 * Extracts the stack from an instance, e.g.
 * Option[List[Int]] => StackHelper {
 * type A = Int
 * type S = Context.Aux[Option] :: Context.Aux[List] :: HNil
 * }
 * TODO: Shouldn't really require everything to be a monad
 */
trait StackHelper[I] {
  type A
  type S <: HList
  type CS <: Context
  val l: Leibniz.===[I, CS#C[A]]
}

trait StackHelper2 {
  implicit def nil[I] = new StackHelper[I] {
    type A = I
    type S = HNil
    type CS = Context.Aux[Id]
    val l = Leibniz.refl[A]
  }
}

object StackHelper extends StackHelper2 {
  implicit def cons[MA, AA](implicit u: Unapply[Monad, MA] {
    type A = AA
  }, rest: StackHelper[AA]) = new StackHelper[MA] {
    type A = rest.A
    type S = Context.Aux[u.M] :: rest.S
    type CS = Context {
      type C[A] = u.M[rest.CS#C[A]]
    }
    //u.leibniz: MA == u.M[AA]
    //rest.l: AA == rest.CS#C[A]
    //want: MA == u.M[rest.CS#C[A]]
    val l = {
      val step: Leibniz.===[u.M[AA], u.M[rest.CS#C[A]]] = Leibniz.lift[⊥, ⊥, ⊤, ⊤, u.M, AA, rest.CS#C[A]](rest.l)
      Leibniz.trans[⊥, ⊤, MA, u.M[AA], u.M[rest.CS#C[A]]](step, u.leibniz)
    }
  }
}

trait ApplyBind[Idx <: HList, L <: HList, R <: HList] {
  type LCS <: Context
  type RCS <: Context
  type OCS <: Context

  val trans: SuperNaturalTransformation[LCS#C, RCS#C, OCS#C]
}

trait IndexedApplyBind[Idx <: HList] {
  def apply[AA, A1, BB, L <: HList, R <: HList, LICS <: Context, RICS <: Context](f: AA, g: A1 ⇒ BB)(implicit sh1: StackHelper[AA] {
    type A = A1
    type S = L
    type CS = LICS
  }, sh2: StackHelper[BB] {
    type S = R
    type CS = RICS
  }, ab: ApplyBind[Idx, L, R] {
    type LCS = LICS
    type RCS = RICS
  }): ab.OCS#C[sh2.A]

  def partialApply[AA, A1, L <: HList, LICS <: Context](f: AA)(implicit sh1: StackHelper[AA] {
    type A = A1
    type S = L
    type CS = LICS
  }): PartiallyAppliedApplyBind[Idx, A1, L, LICS] = {
    val self = this
    new PartiallyAppliedApplyBind[Idx, A1, L, LICS] {
      def apply[BB, R <: HList, RICS <: Context](g: A1 ⇒ BB)(implicit sh2: StackHelper[BB] {
        type S = R
        type CS = RICS
      }, ab: ApplyBind[Idx, L, R] {
        type LCS = LICS
        type RCS = RICS
      }): ab.OCS#C[sh2.A] =
        self.apply(f, g)
    }
  }
}

trait PartiallyAppliedApplyBind[Idx <: HList, A1, L <: HList, LICS <: Context] {
  def apply[BB, R <: HList, RICS <: Context](g: A1 ⇒ BB)(implicit sh2: StackHelper[BB] {
    type S = R
    type CS = RICS
  }, ab: ApplyBind[Idx, L, R] {
    type LCS = LICS
    type RCS = RICS
  }): ab.OCS#C[sh2.A]
}

object ApplyBind {
  implicit def combine[Idx <: HList, L <: HList, LICS <: Context, OL <: HList, LOCS <: Context, R <: HList, RICS <: Context, OR <: HList, ROCS <: Context, FCS <: Context, RFCS <: Context](
    implicit LSS: SelectionSort[Idx, L] {
      type ICS = LICS
      type OCS = LOCS
      type O = OL
    }, LN: Normalizer[Idx, OL] {
      type ICS = LOCS
      type OCS = FCS
    }, RSS: SelectionSort[Idx, R] {
      type ICS = RICS
      type OCS = ROCS
      type O = OR
    }, RN: Normalizer[Idx, OR] {
      type ICS = ROCS
      type OCS = RFCS
    }, stack: MonadStack[Idx] {
      type CS = FCS
    }, w: Leib1[RFCS, FCS]) =
    new ApplyBind[Idx, L, R] {
      type LCS = LICS
      type RCS = RICS
      type OCS = FCS

      val trans = new SuperNaturalTransformation[LCS#C, RCS#C, OCS#C] {
        def apply[A, B](f: LCS#C[A])(g: A ⇒ RCS#C[B]) = {
          implicit val m = stack.m
          LN.trans.apply(LSS.trans.apply(f)) >>= {
            a: A ⇒ w.witness(RN.trans.apply(RSS.trans.apply(g(a))))
          }
        }
      }
    }
  def forIdx[Idx <: HList]: IndexedApplyBind[Idx] = new IndexedApplyBind[Idx] {
    def apply[AA, A1, BB, L <: HList, R <: HList, LICS <: Context, RICS <: Context](f: AA, g: A1 ⇒ BB)(implicit sh1: StackHelper[AA] {
      type A = A1
      type S = L
      type CS = LICS
    }, sh2: StackHelper[BB] {
      type S = R
      type CS = RICS
    }, ab: ApplyBind[Idx, L, R] {
      type LCS = LICS
      type RCS = RICS
    }): ab.OCS#C[sh2.A] = ab.trans(sh1.l.apply(f))({ a ⇒ sh2.l.apply(g(a)) })
  }
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
