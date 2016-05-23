name := "SudSol"

version := "1.0"

scalaVersion := "2.11.8"
libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "2.35.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6"
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.1"

libraryDependencies += "org.scala-lang.modules" % "scala-async_2.11" % "0.9.5"