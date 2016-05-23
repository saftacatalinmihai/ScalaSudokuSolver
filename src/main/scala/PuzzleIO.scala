import java.util.Calendar

import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.openqa.selenium.{WebElement, By, WebDriver}
import org.scalatest.selenium.{Firefox, WebBrowser}
import org.scalatest.{FlatSpec}

import scala.io.Source

/**
  * Created by casafta on 17/5/2016.
  */


object FilePuzzleIO {

  def read(file: String): Puzzle = {

    val pattern = """(\d|_)[^,]*,+""".r
    val lines = for(line <- Source.fromFile(file).getLines())
      yield {
        pattern
          .findAllIn(line)
          .matchData
          .map(_.group(1))
          .toList
      }

    val parsed = lines
      .filter(_.nonEmpty)
      .map(
        _.map(
          token => {
            if (token.matches("\\d"))
              Cell(token.toInt)
            else
              Cell()
          }
        )
      )
      .toList

    val cellsToSet = for {
      i <- 1 to 9
      j <- 1 to 9
      parsedCell = parsed(i-1)(j-1)
      if parsedCell.isKnown
    } yield {
      Map(Loc(i,j) -> parsedCell.value)
    }
    cellsToSet
      .foldLeft(Puzzle())(
        (p, cellMap) => p.setCell(cellMap.keys.head, cellMap.values.head)
      )
  }
  def write = ???

  def main(args: Array[String]) {
    println(read("test.puzzle"))
  }
}

object WebPuzzleIO extends FlatSpec{
  implicit val webDriver: WebDriver = new HtmlUnitDriver
//  System.setProperty("webdriver.chrome.driver", "C:\\Users\\casafta\\Downloads\\chromedriver_win32\\chromedriver.exe")
//  val webDriver = new ChromeDriver
  webDriver.get("http://view.websudoku.com/?level=1")

  def read: Puzzle = {
    val cellMaps = for {
      i <- 0 to 8
      j <- 0 to 8
      inputElement = webDriver.findElement(By.id(s"f$i$j"))
      if !inputElement.getAttribute("value").isEmpty
    } yield {
        Map(Loc(i+1, j+1) ->inputElement.getAttribute("value").toInt)
    }
    cellMaps
      .foldLeft(Puzzle())(
      (p, cellMap) => p.setCell(cellMap.keys.head, cellMap.values.head))
  }

  def writeAndTest(p: Puzzle): Boolean = {
    for {
      j <- 0 to 8
      i <- 0 to 8
      inputElement = webDriver.findElement(By.id(s"f$i$j"))
      if inputElement.getAttribute("value").isEmpty
    } {
      inputElement.sendKeys(p.cells(Loc(i + 1, j + 1)).value.toString)
    }
    val howAmIDoing = webDriver.findElement(By.name("submit"))
    howAmIDoing.click()

    val msg = webDriver.findElements(By.tagName("b")).get(19).getText
    println(msg)
    if (msg.startsWith("""Congratulations"""))
      true
    else
      false
  }

  def main(args: Array[String]) {
    while(true){
      val p = WebPuzzleIO.read
      println("Start: " + Calendar.getInstance().getTime)
      val solved = Puzzle.solve2(p)
      assert(WebPuzzleIO.writeAndTest(solved))
      println("Done: " + Calendar.getInstance().getTime)

      WebPuzzleIO.webDriver.findElement(By.name("newgame")).click()
    }
  }
}