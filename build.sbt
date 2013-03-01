organization := "monitoring"

name := "monitoring"

scalaVersion := "2.10.0"

// Libraries
libraryDependencies ++= Seq(    
    "com.typesafe.akka" %% "akka-actor" % "2.1.1" % "provided",
    "javax.servlet" % "servlet-api" % "2.5" % "provided"
)

// Test libraries
libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "1.9.1" % "test",
    "log4j" % "log4j" % "1.2.17" % "test"
)
