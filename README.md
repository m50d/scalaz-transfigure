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

 * Add more UnapplyC implicits so that it's usable with more container types
 * Make for/yield sugar work - I think this is probably impossible :(
 * Add support for MonadTrans, or something more generic (e.g. a port of Haskell's Layer)
