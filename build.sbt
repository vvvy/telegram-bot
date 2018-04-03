name := "zclimatebot"

version := "0.1"

scalaVersion := "2.12.4"

val http4sVersion = "0.17.5"
//val catsVersion = "0.9.0"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.2",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,

  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.8.0",

  // Optional for string interpolation to JSON model
  "io.circe" %% "circe-literal" % "0.8.0"
)