ThisBuild / version := "0.2.0"

ThisBuild / scalaVersion := "3.2.1-RC1-bin-20220904-b5fea82-NIGHTLY"

lazy val root = (project in file("."))
  .settings(
    name := "Hypervector",
    libraryDependencies += "be.adamv" %% "macroloop-core" % "0.16.0",
    libraryDependencies += "be.adamv" %% "macroloop-collection" % "0.16.0",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  )
