name := "feup-iart-2014"

scalaVersion := "2.10.4"

version := "1.0"

libraryDependencies += "net.sf.jung" % "jung-graph-impl" % "2.0.1"

libraryDependencies += "net.sf.jung" % "jung-visualization" % "2.0.1"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.4"

libraryDependencies += "com.intellij" % "forms_rt" % "7.0.3"

libraryDependencies += "com.github.wookietreiber" %% "scala-chart" % "0.4.0"

antlr4Settings

antlr4GenVisitor in Antlr4 := true

antlr4GenListener in Antlr4 := false

antlr4PackageName in Antlr4 := Some("pt.up.fe.iart.proj1.parser")