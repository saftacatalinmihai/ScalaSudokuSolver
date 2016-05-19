
import java.util.Calendar

import scala.collection.immutable.HashMap
/**
  * Created by casafta on 16/5/2016.
  */

abstract class Puzzle {

  def cells: Map[Loc, Cell]
  def isSolved: Boolean
  def isImpossible: Boolean
  def setCell(loc: Loc, v: Value): Puzzle = {
//    println(this)
    println(s"Setting $loc to $v")

    // Setting the same cell again does nothing
    if (this.cells(loc).value == v) return this

    val setCellPuzzle: Puzzle = Puzzle(cells + (loc -> Cell(v)))
    Puzzle.dependentLocs(loc).foldLeft(setCellPuzzle)(
      (pAcc, l) => pAcc match {
        case p @ ( _:ImpossiblePuzzle | _:SolvedPuzzle) => p
        case _ => pAcc.cells(l) match {
          case c :KnownCell => if (c.value == v) new ImpossiblePuzzle(pAcc.cells) else pAcc
          case c: ImpossibleCell => new ImpossiblePuzzle(setCellPuzzle.cells)
          case ce: EmptyCell => pAcc.removePossibleValFromCell(l, v)
          case cwv: CellWithVals => pAcc.removePossibleValFromCell(l, v).cells(l) match {
            case c: KnownCell => pAcc.setCell(l, c.value)
            case c: ImpossibleCell => new ImpossiblePuzzle(pAcc.cells)
            case _ => pAcc.removePossibleValFromCell(l, v)
          }
        }
      }
    )
  }
  def removePossibleValFromCell(l: Loc, v: Value): Puzzle =
    Puzzle(cells + (l -> cells(l).removePossibleVal(v)))

  def knownCells = cells.filter( _._2.isKnown )

  override def toString = {
    val strList = for {
      i <- 1 to 9
      j <- 1 to 9
    } yield {
      val str = cells(Loc(i,j)) match {
        case KnownCell(v: Value) =>
          v match {
            case KnownValue(intVal) => "[        " + intVal + "         ] "
            case _ => "[        ?         ] "
          }
        case EmptyCell() => "[* * * * * * * * * ] "
        case CellWithVals(vals) =>
          "[" +
            (1 to 9).map(
                        value => if (vals.contains(Value(value))) value.toString else "*"
                      ).foldRight("")( (s1, s2) => s1 + " " + s2) +
            "] "
        case _ => "[        ?         ] "
      }
      if (j == 9) str + "\n" else str
    }
    "\n"+strList.foldLeft("")( (s1, s2) => s1 + s2)
  }
}

case class UnsolvedPuzzle(cells: Map[Loc, Cell]) extends Puzzle{
  require(Puzzle.isPossible(cells), "This puzzle is impossible" + this)

  def isSolved = false
  def isImpossible = false
//  override def removePossibleValFromCell(l: Loc, v: Value): Puzzle = {
//    Puzzle(cells + (l -> cells(l).removePossibleVal(v)))
//  }
//  override def setCell(l: Loc, v: Value): Puzzle =
//    Puzzle(cells + (l -> Cell(v)))

}

case class SolvedPuzzle(val cells: Map[Loc, Cell] ) extends Puzzle{
  require(Puzzle.isPossible(cells), "This puzzle is impossible" + this)

  override def isSolved: Boolean = true
  def isImpossible = false
  override def removePossibleValFromCell(l: Loc, v: Value): Puzzle = this
  override def setCell(l: Loc, v: Value): Puzzle = this
  override def toString = {
    val str = for {
      i <- 1 to 9
      j <- 1 to 9

    } yield {
      val str =  "[" + cells(Loc(i, j)) + "]"
      if (j == 9) str + "\n" else str
    }
    str.foldLeft("")(
      (s1, s2) => s1 + "" + s2
    )
  }
}

case class ImpossiblePuzzle(val cells: Map[Loc, Cell]) extends Puzzle{
  override def isSolved: Boolean = false
  def isImpossible = true
  override def removePossibleValFromCell(l: Loc, v: Value): Puzzle = this
  override def setCell(l: Loc, v: Value): Puzzle = this
}

case class Pair(l: Loc, c: Cell)

object Puzzle {

  def which(p: Puzzle): String =
    p match {
      case ImpossiblePuzzle(_) => "Impossible"
      case UnsolvedPuzzle(_) => "Unsolved"
      case SolvedPuzzle(_) => "Solved"
    }

  def isPossible(cells: Map[Loc, Cell]): Boolean = {
    cells.forall(
      cellMap => {
        val (loc, cell) = (cellMap._1, cellMap._2)
        val depCells = Puzzle.dependentLocs(loc)
        depCells.filter(l => cells(l).isKnown).forall(
          depLoc => cells(depLoc).value != cell.value
        )
      }
    )
  }

  type CellList = List[Map[Loc, Cell]]
  def areAllCellsKnown(p: Puzzle): Boolean =
    p.cells.forall {
      (m) => {
        val (_, c) = (m._1, m._2)
        c.isKnown
      }
    }

  // TODO: tail recursive with acc
  def solve(p: Puzzle, solved: List[Puzzle] = List()): List[Puzzle] = {
//    println(p)
    p match {
      case p: SolvedPuzzle => p :: solved
      case p: ImpossiblePuzzle => solved
      case p: UnsolvedPuzzle =>

        val locCellWithLeastPossibleOptions = Puzzle.cellsByLeastPossibleVals(p).head
        val loc = locCellWithLeastPossibleOptions.keys.head
        val cell = locCellWithLeastPossibleOptions.values.head
        cell.possibleVals.map(
          possibleValue => {
            p.setCell(loc, possibleValue)
          }
        ).flatMap(Puzzle.solve(_))
      }
  }

  def cellsByLeastPossibleVals(p: Puzzle): CellList =
    p.cells
      .filter( k => !p.cells(k._1).isKnown)
      .keys.map(
      loc => HashMap(loc -> p.cells(loc))
    )
    .toList
    .sortBy(
      m => m.values.head.possibleVals.length
    )

  var depLocCache: Map[Loc, List[Loc]] = Map()
  def dependentLocs(l: Loc): List[Loc] = {
    depLocCache.get(l) match {
      case Some(list) =>  list
      case _ =>
        val (row, col) = (l.i, l.j)
        val rowLocs = (1 to 9).filter(_ != row).map(Loc(_, col))
        val colLocs = (1 to 9).filter(_ != col).map(Loc(row, _))
        val blockLocs = {
          val b_row = (row - 1) / 3
          val b_col = (col - 1) / 3
          for {
            i <- (3 * b_row + 1) until (3 * b_row + 4)
            j <- (3 * b_col + 1) until (3 * b_col + 4)
          } yield Loc(i, j)
        }
        depLocCache = depLocCache + (
            l -> (rowLocs ++ colLocs ++ blockLocs).distinct.filter(_!=l).toList
          )
        depLocCache(l)
    }

  }

  def apply(cells: Map[Loc, Cell]): Puzzle =  {
    if (cells.values.forall(_.isKnown)) new SolvedPuzzle(cells)
    else if (cells.values.exists(_.isImpossible)) new ImpossiblePuzzle(cells)
    else if (!Puzzle.isPossible(cells)) new ImpossiblePuzzle(cells)
    else new UnsolvedPuzzle(cells)
  }

  def apply(): Puzzle = {
    new UnsolvedPuzzle(
      (1 to 9).flatMap(
        row => {
          (1 to 9).map(
            (col) => (row, col)
          )
        }
      ).foldLeft(Map[Loc, Cell]())(
        (m, t) => m + (Loc(t._1, t._2) -> Cell())
      ))
  }
}

object Test{
  def main(args: Array[String]) {

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
    assert(Cell().value == ValueNotKnown())
    assert(Cell(1).value == KnownValue(1))
    assert(Cell(4).value == KnownValue(4))
    assert(Cell(5).value == KnownValue(5))
    assert(Cell(1,2).value == ValueNotKnown())
    assert(Loc(2,1) == Loc(2, 1))

    println("Start: " + Calendar.getInstance().getTime)

    val pTest2 = FilePuzzleIO.read("test.puzzle")
    println(pTest2)
    assert(!pTest2.isSolved)

    val solved = Puzzle.solve(pTest2)
    println(solved)
    solved.foreach(p => assert(p.isSolved))
    println("End: " + Calendar.getInstance().getTime)
//
//    val pHardest = FilePuzzleIO.read("hardest.puzzle")
//    println("hardest puzzle start: " + Calendar.getInstance().getTime)
//    Puzzle.solve(pHardest).foreach(
//      p => {
//        println(p)
//        assert(p.isSolved)
//      }
//    )
//    println("hardest puzzle end: " + Calendar.getInstance().getTime)
  }
}