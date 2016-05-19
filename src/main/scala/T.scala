import org.openqa.selenium.{WebDriver, By}
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.selenium.WebBrowser

/**
  * Created by casafta on 18/5/2016.
  */
class T extends FlatSpec with Matchers with WebBrowser{
  implicit val webDriver: WebDriver = new HtmlUnitDriver

  "The blog app home page" should "have the correct title" in {
    go to "http://view.websudoku.com"

    for {
      i <- 0 to 8
      j <- 0 to 8
    } {
      val inputElement = webDriver.findElement(By.id(s"f$i$j"))
      println(inputElement)
      if (!inputElement.getAttribute("value").isEmpty)
        println(inputElement.getAttribute("value"))
    }

    pageTitle should be ("Web Sudoku - Billions of Free Sudoku Puzzles to Play Online")
  }
}

