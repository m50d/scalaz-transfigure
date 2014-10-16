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

 * Allow an "inert" typed value at the "bottom" of the stack - currently we assume any 1-parameter type is a context
 * There's an argument that this code would benefit from reintroducing kind-projector
 * Add suitable error messages where possible (@implicitNotFound)
 * Make it possible to use transfigureTo with for/yield sugar, if possible.
 * Add support for MonadTrans, or something more generic (e.g. a port of Haskell's Layer)
