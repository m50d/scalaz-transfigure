package scalaz

import org.specs2._

class TransfigureToMacroSpec extends mutable.Specification {
  "TransfigureMacro" should {
    "sublists of size" in {
      TransfigureToMacro.sublistsOfSize(1)(List(1)) === List(List(1))
    }
    "small sublist" in {
      TransfigureToMacro.sublistPairs(List(1)) === List((Nil, Nil), (List(1), Nil), (Nil, List(1)), (List(1), List(1)))
    }
    "large sublist" in {
      TransfigureToMacro.sublistPairs(List(1, 2, 3)).take(22) === List(
        (Nil, Nil),
        (List(3), Nil), (List(2), Nil), (List(1), Nil), (Nil, List(3)), (Nil, List(2)), (Nil, List(1)),
        (List(2, 3), Nil), (List(1, 3), Nil), (List(1, 2), Nil), (List(3), List(3)), (List(3), List(2)), (List(3), List(1)),
        (List(2), List(3)), (List(2), List(2)), (List(2), List(1)), (List(1), List(3)), (List(1), List(2)), (List(1), List(1)),
        (Nil, List(2, 3)), (Nil, List(1, 3)), (Nil, List(1, 2)))
    }
  }
}