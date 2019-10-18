organization := "com.lucianomolinari"

name := "shapeless_guide"

version := "1.0"

scalaVersion := "2.12.10"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" %% "cats-core" % "2.0.0"
)