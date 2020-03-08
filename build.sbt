ThisBuild / scalaVersion := "2.13.1"
ThisBuild / organization := "de.leopoldluley"

lazy val lysin = (project in file("."))
    .settings(
        name := "Lysin",
        libraryDependencies += "org.scala-lang.modules"
                            %% "scala-parser-combinators"
                             % "1.1.2",
        libraryDependencies += "com.davegurnell"
                            %% "unindent"
                             % "1.2.0",
    )

