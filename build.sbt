val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fpscala",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    unmanagedSources / excludeFilter := "Monad.scala" || "Monoid.scala" || "Cafe.scala"
    // unmanagedSourceDirectories / excludeFilter := HiddenFileFilter || "chap10" || "chap11"
  )
