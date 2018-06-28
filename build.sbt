name := "wExchange"

version := "0.1"

scalaVersion := "2.12.6"


libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.5.13",

  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "ch.qos.logback" % "logback-classic" % "1.1.7",

  "com.softwaremill.macwire" %% "macros" % "2.3.0" ,
  "com.softwaremill.macwire" %% "util" % "2.3.0" ,

  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "junit" % "junit" % "4.11" % "test",
  "org.mockito" % "mockito-all" % "1.10.19" % "test"
)