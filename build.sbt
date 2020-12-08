val dottyVersion = "3.0.0-M2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent2020",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
