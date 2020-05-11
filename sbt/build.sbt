name := "Scala Playground"

version := "1.0"

scalaVersion := "2.11.11"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2"
/*libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.2"*/
/*libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.3"*/
/*libraryDependencies += "com.trueaccord.scalapb" %% "scalapb-json4s" % "0.3.2"*/
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0"

initialCommands in console += 
"""
|import breeze.linalg._
|import scalaz._
|""".stripMargin
