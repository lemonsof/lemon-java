import sbt._

object LemonProjectBuild extends Build {

  lazy val builder = Project(id = "lemon-builder",base = file("./")).dependsOn(bootstrapCompilers,bootstrapSBT)

  lazy val bootstrapCompilers = Project(id = "lemon-compiler",base = file("lemon-compilers"))

  lazy val bootstrapSBT = Project(id = "lemon-sbt",base = file("lemon-sbt"))
}