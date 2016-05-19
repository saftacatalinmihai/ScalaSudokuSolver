import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by casafta on 19/5/2016.
  */
class PuzzleSpec extends FlatSpec with Matchers{

  "An empty Puzzle" should "be created" in {
    val p = Puzzle()
    p.cells.values.forall(_.possibleVals.length == 9) should be (true)
    p.cells.values.foreach(
      _.possibleVals should have length 9
    )
    p should be (UnsolvedPuzzle(p.cells))
  }

  it should "set the value of a cell and remove that value from dependent cells" in {
    val p = Puzzle()
    val pSet = p.setCell(Loc(1,1), 1)
    pSet.cells(Loc(1, 1)) should be (KnownCell(1))
    for (
      depCell <- Puzzle.dependentLocs(Loc(1,1)).map(pSet.cells(_))
    ) {
      depCell should be (CellWithVals(List(2,3,4,5,6,7,8,9)))
    }
  }

  "A nonempty Puzzle" should "be created" in {
    val p = Puzzle()
      .setCell(Loc(1,1), 1)

    p.knownCells.values.toList.length should be (1)
  }

  it should "do nothing if trying to set the same cell again" in {
    val p = Puzzle()
      .setCell(Loc(1,1), 1)

    val samePuzzle = p.setCell(Loc(1,1),1)
    samePuzzle should be (UnsolvedPuzzle(samePuzzle.cells))
    p should be (samePuzzle)
  }

  ignore should "be set as impossible if trying to set 2 dependent cells to the same value" in {
    val p = Puzzle()

    for {
      i <- 1 to 9
      j <- 1 to 9
      n <- 1 to 9
    } {
      val setPuzzle = p.setCell(Loc(i, j), n)
      for (depLoc <- Puzzle.dependentLocs(Loc(i, j))){
        val impossible = setPuzzle.setCell(depLoc, n)
        impossible should be (ImpossiblePuzzle(impossible.cells))
      }
    }
  }

  "Dependent cells" should "be correct" in {
    for {
      i <- 1 to 9
      j <- 1 to 9
    } {
      val dep = Puzzle.dependentLocs(Loc(i,j))
      dep.forall( l => Puzzle.dependentLocs(l).contains(l))
    }
  }

}
