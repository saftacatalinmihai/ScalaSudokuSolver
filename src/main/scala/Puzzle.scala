
import rx.lang.scala.schedulers.ComputationScheduler
import rx.schedulers.Schedulers

import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import rx.lang.scala.{Subscription, Subscriber, Observable}
import scala.async.Async.{async, await}
import scala.concurrent._
import scala.collection.immutable.HashMap
import scala.util.{Failure, Success}

/**
  * Created by casafta on 16/5/2016.
  */

abstract class Puzzle {

  def cells: Map[Loc, Cell]
  def isSolved: Boolean
  def isImpossible: Boolean
  def setCell(loc: Loc, v: Value): Puzzle = {
//    println(this)
//    println(s"Setting $loc to $v")
    cells(loc) match {
      case c: KnownCell => if (c.value == v) this else new ImpossiblePuzzle(this.cells)
      case c: ImpossibleCell => this
      case _ =>
        val setCellPuzzle: Puzzle = Puzzle(cells + (loc -> Cell(v)))
        Puzzle.dependentLocs(loc).foldLeft(setCellPuzzle)(
          (pAcc, l) => pAcc match {
            case p@(_: ImpossiblePuzzle | _: SolvedPuzzle) => p
            case _ => pAcc.cells(l) match {
              case c: KnownCell => if (c.value == v) new ImpossiblePuzzle(pAcc.cells) else pAcc
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
}

case class SolvedPuzzle(val cells: Map[Loc, Cell] ) extends Puzzle{
  require(Puzzle.isPossible(cells), "This puzzle is impossible" + this)

  override def isSolved: Boolean = true
  def isImpossible = false
  override def removePossibleValFromCell(l: Loc, v: Value): Puzzle = this
  override def setCell(l: Loc, v: Value): Puzzle = this
  override def toString = {
    val headerAndFooter = "+-----------------------+\n"
    val midSection =  "|-------+-------+-------|\n"
    val str =
      for {
      i <- 1 to 9
      j <- 1 to 9
    } yield {
      val str =
        {
          if (j == 1 && (i == 4 || i == 7))
            midSection
          else
            ""
        } +
        {
          if (j == 1 )
            "|"
          else
            ""
        } +
        cells(Loc(i, j)) +
        {
          if (j == 3 || j == 6 )
            " |"
          else if ( j == 9)
            " |\n"
          else
            ""
        }
      str
    }
    "\n" +
    headerAndFooter +
      str.foldLeft("")(
          (s1, s2) => s1 + "" + s2
      ) +
    headerAndFooter
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
  def merge( m1: Map[Loc, Cell], m2: Map[Loc, Cell]): Map[Loc, Cell] = {
    m1.foldLeft(m2)(
      (mergedMapping, newMapping) => {
        newMapping match {
          case (loc, cell) =>
            if (mergedMapping.contains(loc)) {
              val currentCellPossibleVals = mergedMapping(loc).possibleVals.toSet
              val newCellPossibleVals = cell.possibleVals.toSet
              val merged = currentCellPossibleVals.intersect(newCellPossibleVals)
              mergedMapping + (loc -> Cell(merged.toList))
            }
            else mergedMapping + (loc -> cell)
        }
      }
    )
  }

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
  var flatMappingAlgorithm: (List[Puzzle], List[Puzzle]) => List[Puzzle] =
    (solved, possiblePuzzle) => {
      possiblePuzzle.flatMap(Puzzle.solve(_, solved))
    }

  def solve(p: Puzzle, solved: List[Puzzle] = List()): List[Puzzle] = {

    p match {
      case p: SolvedPuzzle =>
//        println(p)
        p :: solved
      case p: ImpossiblePuzzle => solved
      case p: UnsolvedPuzzle =>

        val locCellWithLeastPossibleOptions = Puzzle.cellsByLeastPossibleVals(p).head
        val loc = locCellWithLeastPossibleOptions.keys.head
        val cell = locCellWithLeastPossibleOptions.values.head
        val possiblePuzzles = cell.possibleVals.map(
            possibleValue => {
              p.setCell(loc, possibleValue)
            }
          )

        if (possiblePuzzles.exists(_.isSolved)) {
//          println(possiblePuzzles)
          possiblePuzzles.filter(_.isSolved)
        }
        else {
          for {
            p <- possiblePuzzles
            candidate <- Puzzle.solve(p)
            if candidate.isSolved
          } List(candidate)
        }
        List()

      //        flatMappingAlgorithm(
//          solved,
//          cell.possibleVals.map(
//            possibleValue => {
//              p.setCell(loc, possibleValue)
//            }
//          )
//        )
//        ).flatMap(Puzzle.solve(_, solved))
      }
  }

  def solve2(p: Puzzle): Puzzle = {
    p match {
      case p: SolvedPuzzle => p
      case p: ImpossiblePuzzle => p
      case pu: UnsolvedPuzzle =>

        val locCellWithLeastPossibleOptions = Puzzle.cellsByLeastPossibleVals(pu).head
        val loc = locCellWithLeastPossibleOptions.keys.head
        val cell = locCellWithLeastPossibleOptions.values.head

        for {
          v <- cell.possibleVals
          candidate = pu.setCell(loc, v)
        } {
          if (candidate.isSolved ) return candidate
          else {
            val solvedCandidate = Puzzle.solve2(candidate)
            if (solvedCandidate.isSolved) return solvedCandidate
          }
        }

        Puzzle()
    }
  }

  def solve3(p: Puzzle,
             onFound: (Puzzle) => Unit
            ): Unit = {
    p match {
      case p: SolvedPuzzle => onFound(p)
      case p: ImpossiblePuzzle =>
      case pu: UnsolvedPuzzle =>

        val locCellWithLeastPossibleOptions = Puzzle.cellsByLeastPossibleVals(pu).head
        val loc = locCellWithLeastPossibleOptions.keys.head
        val cell = locCellWithLeastPossibleOptions.values.head

        for {
          v <- cell.possibleVals
          candidate = pu.setCell(loc, v)
        } {
          if (candidate.isSolved ) onFound(candidate)
          else {
            Future {
              Puzzle.solve3(
                candidate,
//                onFound
                (solvedCandidate) => if (solvedCandidate.isSolved) onFound(solvedCandidate)
              )
            }
          }
        }
    }
  }

  def solve4(p: Puzzle): Observable[Puzzle] = {
    p match {
      case p: SolvedPuzzle => Observable.just(p)
      case p: ImpossiblePuzzle => Observable.empty
      case unsolvedPuzzle: UnsolvedPuzzle =>

        Observable
          .from(Puzzle.cellsByLeastPossibleVals(unsolvedPuzzle).head )
          .flatMap( lc => {
            val (loc, cell) = (lc._1, lc._2)
            Observable.from(cell.possibleVals.map(
              pv => unsolvedPuzzle.setCell(loc, pv)
            ))
          })
          .subscribeOn( ComputationScheduler() )
          .flatMap(Puzzle.solve4)
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
        m => m.values.head.possibleVals.size
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
//    val p = Puzzle()
    val p = FilePuzzleIO.read("hardest.puzzle")
//    val p = FilePuzzleIO.read("test.puzzle")

    println("Start")
//    val vList = List(1,9).map(Value(_))
//    val c = Cell(vList)
    val t0 = System.nanoTime()
//    c.removePossibleVal(1)
//    c.removePossibleVal(8)
//    p.setCell(Loc(1,2),1)
//    val solved = Puzzle.solve(p)
//    val t1 = System.nanoTime()
//    println("Elapsed time: " + (t1 - t0) + " ns")
//    println(solved)
      Puzzle.solve4(p).take(1).subscribe((p) => {
        println(p)
        val t1 = System.nanoTime()
        println("Elapsed time: " + (t1 - t0) / 1000000000 + " s")
      }
    )

    scala.io.StdIn.readLine()

//    val o = Observable[Int]( s  => {
//      s.onNext(1)
//      s.onCompleted()
//    })
//
//    val o2 = Observable[Int]( s  => {
//      s.onNext(2)
//      s.onCompleted()
//    })
//
//    val o3 = o ++ o2
//    println("11")
//
//    val future = async {
//      val f1 = async { Thread.sleep(1000); true}
//      val f2 = async { Thread.sleep(2000); 42}
//      if (await(f1)) await(f2) else 0
//    }
//    future.onComplete {
//      case r: Success() => println(r)
//      case f: Failure => println(f)
//    }
//
//    o3.subscribe(println(_))
//    println("22")
  }
}