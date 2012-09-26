scalaVersion := "2.9.1"

organization := "me.lessis"

name := "manuel"

version := "0.1.0-SNAPSHOT"

libraryDependencies += "com.tristanhunt" %% "knockoff" % "0.8.0-16"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies += "com.github.spullara.mustache.java" % "compiler" % "0.8.6"
