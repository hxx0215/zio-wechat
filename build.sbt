import sbt.Keys.libraryDependencies

name := "zio-wechat"

version := "0.1"

scalaVersion := "2.13.3"

val zioVersion = "1.0.1"
val tapirVersion = "0.16.16"
val circeVersion = "0.13.0"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe-yaml" % tapirVersion,
  "com.softwaremill.sttp.tapir" %% "tapir-sttp-client" % tapirVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "com.typesafe.akka" %% "akka-http" % "10.2.1",
  "com.softwaremill.sttp.tapir" %% "tapir-akka-http-server" % tapirVersion,
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  "com.chuusai" %% "shapeless" % "2.3.3"

)
