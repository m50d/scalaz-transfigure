package scalaz

import org.specs2._

class TransfigureSpec extends mutable.Specification {
  "TransfigureMacro" should {
    "sublists of size" in {
      TransfigureToMacro.sublistsOfSize(1)(List(1)) === List(List(1))
    }
    "small sublist" in {
      TransfigureToMacro.sublistPairs(List(1)) === List((Nil, Nil), (List(1), Nil), (Nil, List(1)), (List(1), List(1)))
    }
  }
}