name := "shapeless-akka-stream"

version := "0.1.0"

organization := "com.kreactive"

scalaVersion := "2.12.0"

//scalaOrganization := "org.typelevel"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.4.12",
  "com.chuusai" %% "shapeless" % "2.3.2"
)