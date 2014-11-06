package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("block is standing") {
    new Level1{
      assert(Block(Pos(0,0), Pos(0,0)).isStanding)
    }
  }

  test("block is not standing") {
    new Level1{
      assert(Block(Pos(0,0), Pos(0,1)).isStanding === false)
      assert(Block(Pos(2,2), Pos(2,3)).isStanding === false)
    }
  }

  test("block inside of terrain is legal") {
    new Level1{
      assert(Block(Pos(0,0), Pos(0,0)).isLegal, "0,0;0,0 is legal")
      assert(Block(Pos(2,5), Pos(2,6)).isLegal, "2,5;2,6 is legal")
    }
  }

  test("block outside of terrain is legal") {
    new Level1{
      assert(Block(Pos(0,3), Pos(0,4)).isLegal === false, "0,3;0,4 is not legal")
      assert(Block(Pos(0,2), Pos(0,3)).isLegal === false, "0,2;0,3 is not legal")
      assert(Block(Pos(0,3), Pos(0,3)).isLegal === false, "0,3;0,3 is not legal")
      assert(Block(Pos(2,0), Pos(3,0)).isLegal === false, "2,0;3,0 is not legal")
    }
  }

  test("create block at starting position") {
    new Level1 {
      val blockAtStart = startBlock
      assert(blockAtStart.b1.x === 1, "b1.x === 1")
      assert(blockAtStart.b1.y === 1, "b1.y === 1")
      assert(blockAtStart.b2.x === 1, "b2.x === 1")
      assert(blockAtStart.b2.y === 1, "b2.y === 1")
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
