organization := "monitoring"

name := "monitoring"

scalaVersion := "2.9.2"

// Libraries
libraryDependencies ++= Seq(    
    "com.typesafe.akka" % "akka-actor" % "2.0" % "provided"
)

// Test libraries
libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.6.1" % "test",
    "log4j" % "log4j" % "1.2.17" % "test"
)
