val spire = "org.typelevel" %% "spire" % "0.14.1" 
val scalagraph = "org.scala-graph" %% "graph-core" % "1.12.4"

lazy val root = (project in file("."))
  .settings(
     name := "Elections",
     scalaVersion := "2.12.4",
     libraryDependencies += spire,
     libraryDependencies += scalagraph
  )
