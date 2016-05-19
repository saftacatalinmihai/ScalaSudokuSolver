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

  it should "be set as impossible if trying to set the same cell twice" in {
    val p = Puzzle()
      .setCell(Loc(1,1), 1)

    val impossiblePuzzle = p.setCell(Loc(1,1),1)
    impossiblePuzzle should be (ImpossiblePuzzle(impossiblePuzzle.cells))
  }

  it should "be set as impossible if trying to set 2 dependent cells to the same value" in {
    val p = Puzzle()
      .setCell(Loc(4,5), 1)

    val impossible = p.setCell(Loc(5,6), 1)
    impossible should be (ImpossiblePuzzle(impossible.cells))

  }

}
