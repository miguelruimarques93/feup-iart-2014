name := "feup-iart-2014"

scalaVersion := "2.11.0"

version := "1.0"

libraryDependencies ++= Seq("org.antlr" % "antlr4-runtime" % "4.2.2",
                            "net.sf.jung" % "jung2" % "2.0.1")

unmanagedSourceDirectories in Compile += baseDirectory.value / "gen" / "main" / "java"