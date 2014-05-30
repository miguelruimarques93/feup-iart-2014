import AssemblyKeys._

name := "feup-iart-2014"

scalaVersion := "2.10.4"

version := "1.0"

libraryDependencies ++= Seq(
    "net.sf.jung" % "jung-graph-impl" % "2.0.1",
    "net.sf.jung" % "jung-visualization" % "2.0.1",
    "org.scala-lang" % "scala-swing" % "2.10.4",
    "com.intellij" % "forms_rt" % "7.0.3",
    "com.github.wookietreiber" %% "scala-chart" % "0.4.0"
)

antlr4Settings

antlr4GenVisitor in Antlr4 := true

antlr4GenListener in Antlr4 := false

antlr4PackageName in Antlr4 := Some("pt.up.fe.iart.proj1.parser")

scalacOptions in (Compile,doc) := Seq("-groups", "-implicits", "-diagrams")

lazy val buildSettings = Seq(
    version := "1.0",
    organization := "pt.up.fe.iart.proj1",
    scalaVersion := "2.10.4"
)

lazy val mStrat = mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
{
    case PathList("com", "intellij", xs @ _*) => MergeStrategy.first
    case x => old(x)
}
}

lazy val root = project.in( file(".") ).
    settings(assemblySettings: _*).
    settings(
        jarName in assembly := "feup-iart-2014.jar",
        mStrat,
        outputPath in assembly := file("bin/feup-iart-2014.jar")
    )