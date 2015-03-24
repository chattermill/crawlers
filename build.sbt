name := "ef"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.3"

libraryDependencies += "com.lihaoyi" %% "upickle" % "0.2.8"

libraryDependencies += "codes.reactive" %% "scala-time" % "0.3.0-SNAPSHOT"
