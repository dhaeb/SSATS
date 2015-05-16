package samples

import de.uni.leipzig.constraintprogramming.{ChessBoardPos, Roessel}
import org.junit.Test
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Assertions, FunSuite}
import de.uni.leipzig.constraintprogramming.ChessBoardColumn._

/**
 * Created by Dan Häberlein on 02.05.15.
 */

@RunWith(classOf[JUnitRunner])
class TestRoessel extends Assertions  {
  @Test def chessBoardPos = {
    /*for {
      i <- 1 until 65
      j <- i until 4097 by 64
    } {assert(ChessBoardPos(fromInt((i - 1) / 8 + 1), (i - 1) % 8 + 1) === ChessBoardPos(j), s"at i: ${i} and j: ${j}"); /*println("j= " + j + " " + Roessel.getChessBoardPosition(j))*/}*/
    assert(ChessBoardPos(1) === ChessBoardPos(A,1))
    assert(ChessBoardPos(8) === ChessBoardPos(H,1))
    assert(ChessBoardPos(32) === ChessBoardPos(H,4))
    assert(ChessBoardPos(3) === ChessBoardPos(C,1))
    assert(ChessBoardPos(64) === ChessBoardPos(H,8))
    assert(ChessBoardPos(65) === ChessBoardPos(A,1))
    assert(ChessBoardPos(28) === ChessBoardPos(D, 4))
  }

  @Test def backToVar = {
    assert(ChessBoardPos(A, 1).toInt === 1)
    assert(ChessBoardPos(H, 1).toInt === 8)
    assert(ChessBoardPos(H, 8).toInt === 64)
    assert(ChessBoardPos(H, 4).toInt === 32)
    assert(ChessBoardPos(D, 4).toInt === 28)
    assert(ChessBoardPos(E, 5).toInt === 37)
    assert(ChessBoardPos(C, 5).toInt === 35)
  }

  @Test def roesselSprungValue = {
    assert(Set(ChessBoardPos(B, 3), ChessBoardPos(C, 2)) === ChessBoardPos(A,1).getRoesselMoves())
    assert(Set(ChessBoardPos(C, 1), ChessBoardPos(C, 3), ChessBoardPos(B, 4)) === ChessBoardPos(A,2).getRoesselMoves())
    assert(Set(ChessBoardPos(C, 3), ChessBoardPos(C, 5), ChessBoardPos(B, 2), ChessBoardPos(B, 6)) === ChessBoardPos(A,4).getRoesselMoves())
    assert(Set(ChessBoardPos(B, 3), ChessBoardPos(B, 5), ChessBoardPos(C, 2), ChessBoardPos(C, 6), ChessBoardPos(E, 2), ChessBoardPos(E, 6), ChessBoardPos(F, 3), ChessBoardPos(F, 5)) === ChessBoardPos(D,4).getRoesselMoves())
  }
}
