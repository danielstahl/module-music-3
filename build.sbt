name := "module-music-3"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.6"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "net.soundmining" %% "soundmining-tools" % "1.0-SNAPSHOT"

libraryDependencies += "net.soundmining" %% "soundmining-modular" % "1.0-SNAPSHOT"

libraryDependencies += "com.illposed.osc" % "javaosc-core" % "0.2"

console / initialCommands := """
    |import net.soundmining._
    |ModuleMusic3.init()
""".trim().stripMargin

console / cleanupCommands += """
    ModuleMusic3.stop()
"""
