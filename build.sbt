name := "arpeggio-i"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.4"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "net.soundmining" %% "soundmining-tools" % "1.0-SNAPSHOT"

libraryDependencies += "net.soundmining" %% "soundmining-modular" % "1.0-SNAPSHOT"

libraryDependencies += "com.illposed.osc" % "javaosc-core" % "0.2"

initialCommands in console := """
    |import net.soundmining._
    |Modular3.init()
""".trim().stripMargin

cleanupCommands in console += """
    Modular3.stop()
"""
