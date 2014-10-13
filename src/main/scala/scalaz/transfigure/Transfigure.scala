package scalaz.transfigure

import shapeless.{ Id ⇒ _, _ }
import ops.hlist.{Length, Prepend}
import scalaz._
import scalaz.Scalaz._
import Nat._0
import scala.language.postfixOps
import scala.language.higherKinds

/**
 * Marker that A <= B
 */
sealed trait LE[A <: Nat, B <: Nat]

object LE {
  implicit def le1[B <: Nat] = new LE[_0, B] {}
  implicit def le2[A <: Nat, B <: Nat](implicit le: LE[A, B]) =
    new LE[Succ[A], Succ[B]] {}
}

/**
 * Marker that A > B
 */
sealed trait GT[A <: Nat, B <: Nat]

object GT {
  implicit def gt1[B <: Nat] = new GT[Succ[B], _0] {}
  implicit def gt2[A <: Nat, B <: Nat](implicit gt: GT[A, B]) =
    new GT[Succ[A], Succ[B]] {}
}

/**
 * Extracts the index of A within the list Idx.
 * (Only exists when A is somewhere in Idx)
 * Note that our indices count "backwards":
 * the index of A in A :: B :: HNil is 1,
 * and that of B is 0. I think.
 */
sealed trait IndexOf[Idx <: HList, A] {
  type Out <: Nat
}

trait IndexOf2 {
  implicit def cons[A, B, Remainder <: HList](implicit i: IndexOf[Remainder, A]) = new IndexOf[B :: Remainder, A] {
    type Out = i.Out
  }
}

object IndexOf extends IndexOf2 {
  implicit def head[A, Remainder <: HList](implicit length: Length[Remainder]) = new IndexOf[A :: Remainder, A] {
    type Out = length.Out
  }

  type Aux[Idx <: HList, A, N <: Nat] = IndexOf[Idx, A] { type Out = N }
  def apply[Idx <: HList, A](implicit io: IndexOf[Idx, A]): Aux[Idx, A, io.Out] = io
}

/**
 * Helper for type inference.
 * Expresses that A comes after B in Idx (or A = B)
 * if the type Out exists.
 */
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

/**
 * Helper for type inference.
 * Expresses that A comes before B in Idx
 * if the type Out exists.
 */
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

/**
 * Marker that A comes after B in Idx (or A = B)
 */
sealed trait LEIndexed[Idx <: HList, A, B]
object LEIndexed {
  implicit def ltEqIndexed[Idx <: HList, A, B, O <: LE[_ <: Nat, _ <: Nat]](implicit i: IdxAndLtEq.Aux[Idx, A, B, O], l: O) =
    new LEIndexed[Idx, A, B] {}
}

/**
 * Marker that B comes after A in Idx
 */
sealed trait GTIndexed[Idx <: HList, A, B]
object GTIndexed {
  implicit def gtIndexed[Idx <: HList, A, B, O <: GT[_ <: Nat, _ <: Nat]](implicit i: IdxAndGt.Aux[Idx, A, B, O], l: O) =
    new GTIndexed[Idx, A, B] {}
}

/**
 * Wrapper for a 1-parameter type
 * i.e. a type of kind * -> *.
 * We keep them wrapped in this form to allow type inference to work correctly
 * and to make it easier to build HLists of them.
 */
trait Context {
  type C[_]
}

object Context {
  type Aux[N[_]] = Context {
    type C[A] = N[A]
  }
  type Id = Aux[scalaz.Id.Id]
}

/**
 * Represents a step in sorting a stack of contexts according to the given Idx.
 * trans will be a transformation from C1#C[D#C[A]] to either the same thing
 * or to D#C[C1#C[A]], depending on which of C1 or D comes first in Idx.
 */
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

/**
 * Pulls the earliest context in L to the top of the stack of contexts.
 * i.e. trans is a function from F[G[H[A]]] to X[...[A]], where X
 * is whichever of F/G/H comes first in Idx.
 * From here on we have a "dual" representation of context stacks:
 * we can see F[G[H[A]]] as a list Context[F] :: Context[G] :: Context[H] :: HNil,
 * or as a single Context type. We need the former representation to guide type inference
 * and we need the latter to allow us to declare NaturalTransformations.
 * Whenever you see L <: HList it's the first representation,
 * and any *CS <: Context is the second representation.
 * I have tried to use C/D/etc. to denote "single" contexts,
 * and ICS/RCS/etc. to denote "context stack"s like F[G[H[A]]]
 */

sealed trait SelectLeast[Idx <: HList, L <: HList] {
  type C <: Context
  type R <: HList

  type LCS <: Context
  type RCS <: Context

  val trans: NaturalTransformation[LCS#C, ({ type CRCS[A] = C#C[RCS#C[A]] })#CRCS]
}

object SelectLeast {

  implicit def selectLeast1[Idx <: HList, C1 <: Context] = new SelectLeast[Idx, C1 :: HNil] {
    type C = C1
    type R = HNil
    type LCS = C1
    type RCS = Context.Id
    val trans = new NaturalTransformation[LCS#C, ({ type CRCS[A] = C#C[RCS#C[A]] })#CRCS] {
      def apply[A](fa: C#C[A]) = fa
    }
  }

  implicit def selectLeastCons[Idx <: HList, L <: HList, C1 <: Context, D <: Context](
    implicit tl: Aux3[Idx, L, D], step: SelectionStep[Idx, C1, D], f: Functor[C1#C]) =
    new SelectLeast[Idx, C1 :: L] {
      type C = step.X
      type R = step.Y :: tl.R

      type LCS = Context {
        type C[A] = C1#C[tl.LCS#C[A]]
      }
      type RCS = Context {
        type C[A] = step.Y#C[tl.RCS#C[A]]
      }

      val trans = new NaturalTransformation[LCS#C, ({ type CRCS[A] = C#C[RCS#C[A]] })#CRCS] {
        def apply[A](fa: C1#C[tl.LCS#C[A]]) = {
          val ga: C1#C[tl.C#C[tl.RCS#C[A]]] = fa map {
            tl.trans.apply(_)
          }
          step.trans(ga)
        }
      }
    }
  
  type Aux3[Idx <: HList, L <: HList, C1 <: Context] = SelectLeast[Idx, L] {
    type C = C1
  }
  type Aux1[Idx <: HList, L <: HList, C <: Context, R1 <: HList, RCS1 <: Context] = Aux3[Idx, L, C] {
    type R = R1
    type RCS = RCS1
  }

  def selectLeast[Idx <: HList, L <: HList](implicit sl: SelectLeast[Idx, L]): NaturalTransformation[sl.LCS#C, ({ type CRCS[A] = sl.C#C[sl.RCS#C[A]] })#CRCS] =
    sl.trans
}

/**
 * Leibniz equality (i.e. substitutability) for 1-parameter types.
 * See scalaz.Leibniz - in Haskell we could use the same type, but Scala doesn't
 * let us abstract over kind.
 */
sealed trait LeibC[C <: Context, D <: Context] {
  def subst[F[_[_]]](fc: F[C#C]): F[D#C]
  def witness[A](c: C#C[A]): D#C[A] = subst[({ type L[C[_]] = C[A] })#L](c)
}

object LeibC {
  implicit def refl[C <: Context] = new LeibC[C, C] {
    def subst[F[_[_]]](fa: F[C#C]) = fa
  }
  def symm[C[_], D[_]](l: LeibC[Context.Aux[C], Context.Aux[D]]): LeibC[Context.Aux[D], Context.Aux[C]] =
    l.subst[({type L[G[_]] = LeibC[Context.Aux[G], Context.Aux[C]]})#L](refl[Context.Aux[C]])
}

/**
 * A complete sort of the context stack L, relative to the index Idx.
 * O is the output type (OCS) expressed as a list of contexts (rather than a single stacked context).
 * (this is useful for guiding inference in later steps)
 */
sealed trait SelectionSort[Idx <: HList, L <: HList] {
  type ICS <: Context
  type O <: HList
  type OCS <: Context

  val trans: NaturalTransformation[ICS#C, OCS#C]
}

object SelectionSort {
  implicit def nil[Idx <: HList] = new SelectionSort[Idx, HNil] {
    type ICS = Context.Id
    type O = HNil
    type OCS = Context.Id

    val trans = new NaturalTransformation[ICS#C, OCS#C] {
      def apply[A](fa: A) = fa
    }
  }

  implicit def cons[Idx <: HList, L <: HList, C1 <: Context, R <: HList, SLR <: Context, TLI <: Context](implicit sl: SelectLeast.Aux1[Idx, L, C1, R, SLR],
    tl: Aux2[Idx, R, TLI], f: Functor[C1#C], w: LeibC[SLR, TLI]) =
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
  
  type Aux2[Idx <: HList, L <: HList, ICS1 <: Context] = SelectionSort[Idx, L] {
    type ICS = ICS1
  }

  type Aux[Idx <: HList, L <: HList, ICS <: Context, O1 <: HList, OCS1 <: Context] = Aux2[Idx, L, ICS] {
    type O = O1
    type OCS = OCS1
  }
  def apply[Idx <: HList, L <: HList](implicit ss: SelectionSort[Idx, L]): Aux[Idx, L, ss.ICS, ss.O, ss.OCS] = ss

  def selectionSort[Idx <: HList, L <: HList](implicit ss: SelectionSort[Idx, L]): NaturalTransformation[ss.ICS#C, ss.OCS#C] =
    ss.trans
}

/**
 * "Normalizes" the list of contexts L to equal the list Idx.
 * L should be sorted relative to Idx beforehand.
 * We represent a normalizer in slightly "peeled" form
 * (the output context stack is really C[RCS[A]])
 * to make it easy to write the "consBind" implementation.
 */
sealed trait Normalizer[Idx <: HList, L <: HList] {
  type ICS <: Context
  type C <: Context
  type RCS <: Context
  type K <: HList

  val trans: NaturalTransformation[ICS#C, ({ type L[A] = C#C[RCS#C[A]] })#L]
}

trait Normalizer5 {
  implicit def nilPoint[C1 <: Context](implicit ap: Applicative[C1#C]) = new Normalizer[C1 :: HNil, HNil] {
    type ICS = Context.Id
    type C = C1
    type RCS = Context.Id
    type K = HNil

    val trans = new NaturalTransformation[ICS#C, C1#C] {
      def apply[A](fa: A) =
        Applicative[C1#C].point(fa)
    }
  }
}

trait Normalizer4 extends Normalizer5 {
  implicit def nilMap[C1 <: Context] = new Normalizer[C1 :: HNil, C1 :: HNil] {
    type ICS = C1
    type C = C1
    type RCS = Context.Id
    type K = HNil

    val trans = new NaturalTransformation[ICS#C, C1#C] {
      def apply[A](fa: ICS#C[A]) = fa
    }
  }
}

trait Normalizer3 extends Normalizer4 {
  implicit def consPoint[C1 <: Context, T <: HList, L <: HList](implicit rest: Normalizer[T, L], ap: Applicative[C1#C]) = new Normalizer[C1 :: T, L] {
    type ICS = rest.ICS
    type C = C1
    type RCS = Context {
      type C[A] = rest.C#C[rest.RCS#C[A]]
    }
    type K = rest.C :: rest.K
    
    val trans = new NaturalTransformation[ICS#C, ({ type L[A] = C#C[RCS#C[A]] })#L] {
      def apply[A](fa: ICS#C[A]) = Applicative[C1#C].point(rest.trans.apply(fa))
    }
  }
}

trait Normalizer2 extends Normalizer3 {
  implicit def consMap[C1 <: Context, T <: HList, L <: HList](implicit rest: Normalizer[T, L], f: Functor[C1#C]) = new Normalizer[C1 :: T, C1 :: L] {
    type ICS = Context {
      type C[A] = C1#C[rest.ICS#C[A]]
    }
    type C = C1
    type RCS = Context {
      type C[A] = rest.C#C[rest.RCS#C[A]]
    }
    type K = rest.C :: rest.K
    
    val trans = new NaturalTransformation[ICS#C, ({ type L[A] = C#C[RCS#C[A]] })#L] {
      def apply[A](fa: ICS#C[A]) = fa map { rest.trans.apply(_) }
    }
  }
}

object Normalizer extends Normalizer2 {
  /**
   * TODO: I think this might require some handholding of the type inference -
   * I suspect it can only find the Normalizer with type C = C1 if
   * it's at the lowest level of the stack.
   * The same technique we use elsewhere (IdxAndLtEq etc.) should work.
   */
  implicit def consBind[C1 <: Context, T <: HList, L <: HList](implicit rest: Normalizer[C1 :: T, C1 :: L] { type C = C1 },
    b: Bind[C1#C]) = new Normalizer[C1 :: T, C1 :: C1 :: L] {
    type ICS = Context {
      type C[A] = C1#C[rest.ICS#C[A]]
    }
    type C = C1
    type RCS = rest.RCS
    type K = rest.K
    
    val trans = new NaturalTransformation[ICS#C, ({ type L[A] = C#C[RCS#C[A]] })#L] {
      def apply[A](fa: ICS#C[A]) =
        fa map { rest.trans.apply(_) } μ
    }
  }
}

/**
 * Helper to guide type inference.
 * Represents a combined sort-and-normalize of the context stack L
 * to the context stack Idx, provided the Leibniz equivalence of
 * type Required exists.
 * In fact this equivalence will always exist (because if ss.O = M
 * then ss.OCS will equal n.ICS, as they're derived in exactly the
 * same way), but the compiler doesn't know that.
 */
sealed trait SortAndNormalizerRequiringLeibniz[Idx <: HList, L <: HList] {
  type ICS <: Context
  type Required <: LeibC[_, _]
  type OCS <: Context
  type K <: HList

  def sort(leib: Required): NaturalTransformation[ICS#C, OCS#C]
}

object SortAndNormalizerRequiringLeibniz {
  implicit def fromSort[Idx <: HList, L <: HList, M <: HList](implicit ss: SelectionSort[Idx, L] { type O = M }, n: Normalizer[Idx, M]) =
    new SortAndNormalizerRequiringLeibniz[Idx, L] {
      type ICS = ss.ICS
      type Required = LeibC[ss.OCS, n.ICS]
      type OCS = Context {
        type C[A] = n.C#C[n.RCS#C[A]]
      }
      type K = n.C :: n.K
      def sort(leib: Required) = new NaturalTransformation[ICS#C, OCS#C] {
        def apply[A](fa: ICS#C[A]) =
          n.trans.apply(leib.witness(ss.trans.apply(fa)))
      }
    }
}

/**
 * A combined sort-and-normalize from the context stack L to the context stack Idx
 */
sealed trait SortAndNormalizer[Idx <: HList, L <: HList] {
  type ICS <: Context
  type OCS <: Context
  type K <: HList

  val trans: NaturalTransformation[ICS#C, OCS#C]
}

object SortAndNormalizer {
  implicit def fromSN[Idx <: HList, L <: HList, R <: LeibC[_, _]](implicit sort: SortAndNormalizerRequiringLeibniz[Idx, L] {
    type Required = R
  }, leib: R) = new SortAndNormalizer[Idx, L] {
    type ICS = sort.ICS
    type OCS = sort.OCS
    type K = sort.K

    val trans = sort.sort(leib)
  }
  type Aux[Idx <: HList, L <: HList, ICS1 <: Context, OCS1 <: Context, K1 <: HList] = SortAndNormalizer[Idx, L] {
    type ICS = ICS1
    type OCS = OCS1
    type K = K1
  }
  def apply[Idx <: HList, L <: HList](implicit sn: SortAndNormalizer[Idx, L]): Aux[Idx, L, sn.ICS, sn.OCS, sn.K] = sn
}

/**
 * Substitute for MonadTrans. Inspired by Haskell's layers package,
 * but not (yet) as general or elegant.
 * Allows us to view a stack of monads (e.g. F[G[A]]) as a single monad,
 * without having to wrap it in another type.
 */
trait Layer[M[_]] {
  def monad[F[_]: Monad]: Monad[({ type L[A] = F[M[A]] })#L]

  def lift[N[_]](other: Layer[N]): Layer[({ type L[A] = N[M[A]] })#L] =
    {
      val self = this
      new Layer[({ type L[A] = N[M[A]] })#L] {
        def monad[F[_]: Monad] = self.monad[({ type L[A] = F[N[A]] })#L](other.monad(Monad[F]))
      }
    }
}

/**
 * Layer instances for a few existing types, mostly cribbed from
 * their respective MonadTrans in scalaz.
 * Note that Layers do not exist for all Monads (e.g. there is
 * no Layer for Future), but they should
 * exist for any Monad for which a MonadTrans exists.
 */
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

  implicit def writerLayer[A: Monoid] = new Layer[({ type L[B] = Writer[A, B] })#L] {
    def monad[F[_]: Monad] = new Monad[({ type L[B] = F[Writer[A, B]] })#L] {
      def point[B](b: ⇒ B) = MonadTell[Writer, A].point(b).point[F]
      def bind[B, C](fb: F[Writer[A, B]])(f: B ⇒ F[Writer[A, C]]) =
        Monad[F].bind(fb) { wa ⇒
          val z = f(wa.run._2)
          Monad[F].map(z)(wb ⇒ Writer(Monoid[A].append(wa.run._1, wb.run._1), wb.run._2))
        }
    }
  }

  implicit def eitherLayer[A] = new Layer[({ type L[B] = Either[A, B] })#L] {
    def monad[F[_]: Monad] = new Monad[({ type L[B] = F[Either[A, B]] })#L] {
      def point[B](b: ⇒ B) = (Right(b): Either[A, B]).point[F]
      def bind[B, C](fb: F[Either[A, B]])(f: B ⇒ F[Either[A, C]]) =
        Monad[F].bind(fb){_.fold(a => Monad[F].point(Left(a): Either[A, C]), b => f(b))}
    }
  }

  implicit def scalazEitherLayer[A] = new Layer[({ type L[B] = A \/ B })#L] {
    def monad[F[_]: Monad] = new Monad[({ type L[B] = F[A \/ B] })#L] {
      def point[B](b: ⇒ B) = (\/-(b): A \/ B).point[F]
      def bind[B, C](fb: F[A \/ B])(f: B ⇒ F[A \/ C]) =
        Monad[F].bind(fb){_.fold(a => Monad[F].point(-\/(a): A \/ C), b => f(b))}
    }
  }
}

/**
 * Converts between the two representations of a context stack:
 * L is a HList of contexts, C[RS[A]] is the same stack as a
 * single composed context. Also contains a monad for the context.
 * We keep the monad stack in slightly "peeled" form
 * so that the outer monad is allowed to be one
 * without a layer (e.g. Future)
 */
sealed trait MonadStack[L <: HList] {
  type C <: Context
  type RS <: Context

  val l: Layer[RS#C]
  val cm: Monad[C#C]
  def m: Monad[({ type L[A] = C#C[RS#C[A]] })#L] = l.monad(cm)
}

trait MonadStack1 {
  implicit def nil[C1 <: Context](implicit m1: Monad[C1#C]) = new MonadStack[C1 :: HNil] {
    type C = C1
    type RS = Context.Id

    val cm = m1
    val l = Layer.IdLayer
  }
}

object MonadStack extends MonadStack1 {
  implicit def cons[C1 <: Context, D <: Context, T <: HList](implicit rest: MonadStack[D :: T] { type C = D }, l1: Layer[D#C], m1: Monad[C1#C]) =
    new MonadStack[C1 :: D :: T] {
      type C = C1
      type RS = Context {
        type C[A] = D#C[rest.RS#C[A]]
      }

      val l = rest.l.lift(l1)
      val cm = m1
    }
  type Aux[L <: HList, C1 <: Context, RS1 <: Context] = MonadStack[L] {
    type C = C1
    type RS = RS1
  }
}

/**
 * Two-argument form of NaturalTransformation.
 */
trait SuperNaturalTransformation[-F[_], -G[_], +H[_]] {
  def apply[A, B](f: F[A])(g: A ⇒ G[B]): H[B]
}

/**
 * Extracts the stack from an instance, e.g.
 * Option[List[Int]] => StackHelper {
 * type A = Int
 * type S = Context.Aux[Option] :: Context.Aux[List] :: HNil
 * }
 */
sealed trait StackHelper[I] {
  type A
  type S <: HList
  type CS <: Context
  val l: Leibniz.===[I, CS#C[A]]
}

trait StackHelper2 {
  implicit def nil[I] = new StackHelper[I] {
    type A = I
    type S = HNil
    type CS = Context.Id
    val l = Leibniz.refl[A]
  }
}

object StackHelper extends StackHelper2 {
  implicit def cons[MA, AA](implicit u: Unapply[Functor, MA] {
    type A = AA
  }, rest: StackHelper[AA]) = new StackHelper[MA] {
    type A = rest.A
    type S = Context.Aux[u.M] :: rest.S
    type CS = Context {
      type C[A] = u.M[rest.CS#C[A]]
    }
    val l = {
      val step: Leibniz.===[u.M[AA], u.M[rest.CS#C[A]]] = Leibniz.lift[⊥, ⊥, ⊤, ⊤, u.M, AA, rest.CS#C[A]](rest.l)
      Leibniz.trans[⊥, ⊤, MA, u.M[AA], u.M[rest.CS#C[A]]](step, u.leibniz)
    }
  }
  
  type Aux1[I, S1, CS1] = StackHelper[I] {
    type S = S1
    type CS = CS1
  }
  
  type Aux[I, A1, S, CS] = Aux1[I, S, CS] {
    type A = A1
  }
}

/**
 * Brings everything together.
 * Provides a transformation between two stacks L, R
 * and a third stack Idx, which first applies
 * a sort-normalize to each of L and R, transforming
 * both into Idx, and then uses a monad stack
 * to combine the two context stacks into a single
 * instance of the same context stack.
 */
sealed trait ApplyBind[Idx <: HList, L <: HList, R <: HList] {
  type LCS <: Context
  type RCS <: Context
  type OCS <: Context

  val trans: SuperNaturalTransformation[LCS#C, RCS#C, OCS#C]
}

/**
 * Partially constructed ApplyBind. Has Idx fixed but the remaining parameters free.
 */
sealed trait IndexedApplyBind[Idx <: HList] {
  /**
   * Apply to both arguments directly. Useful for testing, but no longer used directly in code.
   */
  def apply[AA, A1, BB, L <: HList, R <: HList, LICS <: Context, RICS <: Context](f: AA, g: A1 ⇒ BB)(
      implicit sh1: StackHelper.Aux[AA, A1, L, LICS], sh2: StackHelper.Aux1[BB, R, RICS], ab: ApplyBind[Idx, L, R] {
    type LCS = LICS
    type RCS = RICS
  }): ab.OCS#C[sh2.A]

  /**
   * Apply to one argument, so that it can later be applied to the second argument. This is
   * the method that the main code actually uses.
   */
  def partialApply[AA, A1, L <: HList, LICS <: Context](f: AA)(implicit sh1: StackHelper.Aux[AA, A1, L, LICS]): PartiallyAppliedApplyBind[Idx, A1, L, LICS] = {
    val self = this
    new PartiallyAppliedApplyBind[Idx, A1, L, LICS] {
      def apply[BB, R <: HList, RICS <: Context](g: A1 ⇒ BB)(implicit sh2: StackHelper.Aux1[BB, R, RICS], ab: ApplyBind[Idx, L, R] {
        type LCS = LICS
        type RCS = RICS
      }): ab.OCS#C[sh2.A] =
        self.apply(f, g)
    }
  }
}

/**
 * The remaining implicits needed by a PartiallyAppliedApplyBind
 */
sealed trait RemainingApplication[Idx <: HList, BB, R <: HList]

/**
 * Partially constructed ApplyBind. The index and the left hand side (deconstructed into a context stack)
 * are fixed, but the right hand side is currently free.
 */
sealed trait PartiallyAppliedApplyBind[Idx <: HList, A1, L <: HList, LICS <: Context] {
  def apply[BB, R <: HList, RICS <: Context](g: A1 ⇒ BB)(implicit sh2: StackHelper.Aux1[BB, R, RICS], ab: ApplyBind[Idx, L, R] {
    type LCS = LICS
    type RCS = RICS
  }): ab.OCS#C[sh2.A]
}

object ApplyBind {
  implicit def combine[Idx <: HList, L <: HList, LICS <: Context, LOCS <: Context, LK <: HList, R <: HList, RICS <: Context, ROCS <: Context, RK <: HList, FC <: Context, FRCS <: Context,
    I <: HList, OICS <: Context, OOCS <: Context](
    implicit lsn: SortAndNormalizer[Idx, L] {
      type ICS = LICS
      type OCS = LOCS
      type K = LK
    }, rsn: SortAndNormalizer[Idx, R] {
      type ICS = RICS
      type OCS = ROCS
      type K = RK
    }, stack: MonadStack.Aux[Idx, FC, FRCS],
    w1: LeibC[LOCS, Context.Aux[({ type L[A] = FC#C[FRCS#C[A]] })#L]],
    w2: LeibC[ROCS, Context.Aux[({ type L[A] = FC#C[FRCS#C[A]] })#L]],
    pa: Prepend.Aux[LK, RK, I],
    osn: SortAndNormalizer[Idx, I]{
      type ICS = OICS
      type OCS = OOCS
    },
    w3: LeibC[Context.Aux[({type L[A] = LOCS#C[ROCS#C[A]]})#L], OICS],
    w4: LeibC[OOCS, Context.Aux[({type L[A] = FC#C[FRCS#C[A]]})#L]],
    w5: LeibC[Context.Aux[({ type L[A] = FC#C[FRCS#C[A]] })#L], LOCS]
    ) =
    new ApplyBind[Idx, L, R] {
      type LCS = LICS
      type RCS = RICS
      type OCS = Context {
        type C[A] = FC#C[FRCS#C[A]]
      }

      val trans = new SuperNaturalTransformation[LCS#C, RCS#C, OCS#C] {
        def apply[A, B](f: LCS#C[A])(g: A ⇒ RCS#C[B]) = {
          implicit val n = w5.subst(stack.m)
          val mapped = lsn.trans.apply(f) map {a: A => rsn.trans.apply(g(a))}
          w4.witness(osn.trans.apply(w3.witness(mapped)))
        }
      }
    }

  /**
   * Fix the index stack, and return a partially constructed instance
   * that can be applied to a left stack and a right stack to form
   * the complete ApplyBind
   */
  def forIdx[Idx <: HList]: IndexedApplyBind[Idx] = new IndexedApplyBind[Idx] {
    def apply[AA, A1, BB, L <: HList, R <: HList, LICS <: Context, RICS <: Context](f: AA, g: A1 ⇒ BB)(
        implicit sh1: StackHelper.Aux[AA, A1, L, LICS], sh2: StackHelper.Aux1[BB, R, RICS], ab: ApplyBind[Idx, L, R] {
      type LCS = LICS
      type RCS = RICS
    }): ab.OCS#C[sh2.A] = ab.trans(sh1.l.apply(f))({ a ⇒ sh2.l.apply(g(a)) })
  }
  
  type AnyAux[Idx <: HList, L <: HList, R <: HList, LICS1 <: Context, RICS1 <: Context] = ApplyBind[Idx, L, R] {
    type LICS = LICS1
    type RICS = RICS1
  }
}