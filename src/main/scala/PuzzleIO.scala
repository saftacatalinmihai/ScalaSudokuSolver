import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.firefox.FirefoxDriver
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.openqa.selenium.ie.InternetExplorerDriver
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

    val c = for {
      i <- 1 to 9
      j <- 1 to 9
    } yield {
      Map(Loc(i,j) -> parsed(i-1)(j-1))
    }
    val cells = c.reduce(_ ++ _)
    Puzzle(cells)
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
    } yield {
      val inputElement = webDriver.findElement(By.id(s"f$i$j"))
      if (!inputElement.getAttribute("value").isEmpty)
        Map(Loc(i+1, j+1) -> Cell(inputElement.getAttribute("value").toInt))
      else Map(Loc(i+1, j+1) -> Cell())
    }
    val cells = cellMaps.reduce(_ ++ _)
    Puzzle(cells)
  }

  def writeAndTest(p: Puzzle): Boolean = {
    for {
      i <- 0 to 8
      j <- 0 to 8
    } {
      val inputElement = webDriver.findElement(By.id(s"f$i$j"))
      if (inputElement.getAttribute("value").isEmpty) {
        inputElement.sendKeys(p.cells(Loc(i + 1, j + 1)).value.toString)
      }
    }
    val howAmIDoing = webDriver.findElement(By.name("submit"))
    howAmIDoing.click()

    val msg = webDriver.findElements(By.tagName("b")).get(19).getText
    println(msg)
    if (msg.matches("""Congratulations!.*"""))
      true
    else
      false
  }

  def main(args: Array[String]) {
//    (1 to 10).foreach {_ =>
    while(true){
      val p = WebPuzzleIO.read
      val solved = Puzzle.solve(p)
      if (solved.nonEmpty)
        assert(WebPuzzleIO.writeAndTest(solved.head))
      else println("can't solve")
      WebPuzzleIO.webDriver.findElement(By.name("newgame")).click()
    }

  }
}