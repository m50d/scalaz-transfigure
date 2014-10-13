# scalaz-transfigure

A library for genericly composing/lifting operations in a stack of monadic contexts

## Basic Usage

````scala
import scalaz.transfigure.TransfigureSyntax._
// fa could be an arbitrary stack of List/Option
val fa: List[Option[Int]] = List(Some(42))
// f returns some other arbitrary stack of List/Option
val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)
// transfigureTo returns exactly the requested stack
val g: List[Option[String]] = fa.transfigureTo[List, Option](f)
````

## Todo

 * There's an argument that this code would benefit from reintroducing kind-projector
 * Add suitable error messages where possible (@implicitNotFound)
 * Make it possible to use transfigureTo with for/yield sugar, if possible.
 * Move away from Layer - it currently does nothing that Traverse/Distributive can't
 * Add support for MonadTrans
 * Add support for something more generic (Layer)
 * Verify that we bind repetitive contexts correctly when the repeated context is
 high up in the stack e.g. `Option[Option[List[EitherR[A]]]].transfigureTo3[Option, List, EitherR](identity)`
 * Verify that we allow the stack to contain monads without Traverse instances as long as we don't traverse them
   * It might be possible to replace our use of Traverse entirely by using Layer
   * Or by using Distributive; we should at least allow that as an alternative
 * Try to weaken the assumptions - maybe allow some components of the stack to be Applicative rather than Monad
 if they are never bound