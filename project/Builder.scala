import sbt._

object LemonBuild extends Build {

  lazy val compilers = Project(id = "lemon-compilers",base = file("./project/lemon-compilers"))

  lazy val runtimes = Project(id = "lemon-runtimes",base = file("lemon-runtimes")).dependsOn(compilers)
}