name := "Scala Playground"

version := "1.0"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.2"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.3"
libraryDependencies += "com.trueaccord.scalapb" %% "scalapb-json4s" % "0.3.2"

initialCommands in console += "import breeze.linalg._"
