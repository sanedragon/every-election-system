val scalagraph = "org.scala-graph" %% "graph-core" % "1.12.4"
val scalatest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"
val playjson = "com.typesafe.play" %% "play-json" % "2.7.2"

lazy val root = (project in file("."))
  .settings(
     name := "every-election-system",
     scalaVersion := "2.12.8",
     libraryDependencies += scalagraph,
     libraryDependencies += scalatest,
     libraryDependencies += playjson,
     logBuffered in Test := false
  )
