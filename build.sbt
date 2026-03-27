val scala3Version = "3.8.2"
lazy val root = project
  .in(file("."))
  .settings(
    name := "project-3a",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.2.4" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
    )
  )