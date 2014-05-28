name := "feup-iart-2014"

scalaVersion := "2.10.4"

version := "1.0"

libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.2.2"

libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1"

libraryDependencies += "net.sf.jung" % "jung-visualization" % "2.0.1"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.4"

libraryDependencies += "com.intellij" % "forms_rt" % "7.0.3"

unmanagedSourceDirectories in Compile ++= Seq(
    baseDirectory.value / "gen" / "main" / "java",
    baseDirectory.value / "src" / "main" / "java"
)