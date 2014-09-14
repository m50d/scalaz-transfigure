# scalaz-transfigure

A library for genericly composing/lifting operations in a stack of monadic contexts

## Basic Usage

````scala
import scalaz.transfigure.TransfigureSyntax._
// fa could be an arbitrary stack of List/Option
val fa: List[Option[Int]] = List(Some(42))
// f returns some other arbitrary stack of List/Option
val f: Int ⇒ List[String] = x ⇒ List((x - 10).toString)
// transfigureToX returns exactly the requested stack
val g: List[Option[String]] = fa.transfigureTo2[List, Option](f)
````

## Todo

 * Support stacks of more than 3 monads
 * General tidy-up of code
   * There's an argument that this code would benefit from reintroducing kind-projector
 * Add suitable error messages where possible (@implicitNotFound)
 * Make Layer more consistent with the rest of the world
   * Write Layer instances for more types (e.g. Either)
   * Make it easier to derive Layer from MonadTrans, if possible
   * Follow the Haskell layers library more closely
     * e.g. allow Layer where the outer context is not a Monad in its own right, provided the complete stack forms a Monad
   * Encourage Scalaz to adopt Layer or equivalent
 * Verify that we allow a non-layerable monad (e.g. Future) as the outermost context
 * Verify that we bind repetitive contexts correctly when the repeated context is
 high up in the stack e.g. `Option[Option[List[EitherR[A]]]].transfigureTo3[Option, List, EitherR](identity)`
 * Verify that we allow the stack to contain monads without Traverse instances as long as we don't traverse them
   * It might be possible to replace our use of Traverse entirely by using Layer
 * Verify that we can handle an "inert" parameterized type at the bottom of the stack e.g.
 `Option[Tree[A]].transfigureTo1[Option](...)`
 * Try to weaken the assumptions - maybe allow some components of the stack to be Applicative rather than Monad
 if they are never bound