import sbt._
import sbt.Keys._

object Build extends sbt.Build {
  lazy val lib = Project("lib", file("."))
  lazy val app = Project("app", file("app")) dependsOn(lib)
}
