import java.util.Calendar

import org.openqa.selenium.By
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by casafta on 20/5/2016.
  */

class PuzzleSolve extends FlatSpec with Matchers{

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + " ns")
    result
  }

  "A simple puzzle" should "be solvable" in {
    val pTest = Puzzle()
      .setCell(Loc(1,2), 6)
      .setCell(Loc(1,4), 8)
      .setCell(Loc(1,6), 7)
      .setCell(Loc(2,1), 1)
      .setCell(Loc(2,2), 5)
      .setCell(Loc(2,4), 2)
      .setCell(Loc(2,6), 6)
      .setCell(Loc(2,7), 9)
      .setCell(Loc(2,8), 8)
      .setCell(Loc(3,1), 2)
      .setCell(Loc(3,2), 4)
      .setCell(Loc(3,3), 8)
      .setCell(Loc(3,3), 8)
      .setCell(Loc(4,3), 9)
      .setCell(Loc(4,4), 6)
      .setCell(Loc(4,5), 8)
      .setCell(Loc(4,6), 4)
      .setCell(Loc(6,4), 5)
      .setCell(Loc(6,5), 3)
      .setCell(Loc(6,6), 2)
      .setCell(Loc(6,7), 1)
      .setCell(Loc(7,7), 4)
      .setCell(Loc(7,8), 9)
      .setCell(Loc(7,9), 2)
      .setCell(Loc(8,2), 9)
      .setCell(Loc(8,3), 6)
      .setCell(Loc(8,4), 7)
      .setCell(Loc(8,6), 3)
      .setCell(Loc(8,8), 5)
      .setCell(Loc(8,9), 1)
      .setCell(Loc(9,4), 4)
      .setCell(Loc(9,6), 9)
      .setCell(Loc(9,8), 6)

    assert(pTest.isSolved)
  }

  "A test puzzle of moderate complexity" should "be solvable" in {
    val pTest2 = FilePuzzleIO.read("test.puzzle")
    assert(!pTest2.isSolved)
    time {
      val solved = Puzzle.solve(pTest2)
      println(solved)
      solved.foreach(p => assert(p.isSolved))
    }
  }

  "Puzzles got from websudoku.com" should "be solved" in {
    Puzzle.flatMappingAlgorithm = (solved, possiblePuzzle) =>  {
      possiblePuzzle.par.flatMap(Puzzle.solve(_, solved)).toList
    }
    for ( i <- 1 to 10) {
      time {
        val solved = Puzzle.solve(WebPuzzleIO.read)
        assert(WebPuzzleIO.writeAndTest(solved.head))
      }
      WebPuzzleIO.webDriver.findElement(By.name("newgame")).click()
    }
  }

  "The hardest sudoku puzzle" should "be solved" in {
    val pHardest = FilePuzzleIO.read("hardest.puzzle")
    1 to 10 foreach {
      _ => {
        time {
          Puzzle.solve(pHardest).foreach(
            p => {
              println(p)
              assert(p.isSolved)
            }
          )
        }
      }
    }
  }
}
