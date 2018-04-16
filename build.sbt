val spire = "org.typelevel" %% "spire" % "0.14.1" 
val scalagraph = "org.scala-graph" %% "graph-core" % "1.12.4"
val scalatest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

lazy val root = (project in file("."))
  .settings(
     name := "every-election-system",
     scalaVersion := "2.12.4",
     libraryDependencies += spire,
     libraryDependencies += scalagraph,
     libraryDependencies += scalatest,
     logBuffered in Test := false
  )
