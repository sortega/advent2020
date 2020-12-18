val dottyVersion = "3.0.0-M2"

testFrameworks += new TestFramework("munit.Framework")

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent2020",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value),
      "com.novocode" % "junit-interface" % "0.11" % Test,
      "org.scalameta" %% "munit" % "0.7.19" % Test
    )
  )
