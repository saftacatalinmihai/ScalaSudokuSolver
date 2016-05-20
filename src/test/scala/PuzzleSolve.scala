import org.openqa.selenium.By
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by casafta on 20/5/2016.
  */
class PuzzleSolve extends FlatSpec with Matchers{

  "Puzzles got from websudoku.com" should "be solved" in {
    for ( i <- 1 to 10) {
      val solved = Puzzle.solve(WebPuzzleIO.read)
      assert(WebPuzzleIO.writeAndTest(solved.head))
      WebPuzzleIO.webDriver.findElement(By.name("newgame")).click()
    }
  }
}
