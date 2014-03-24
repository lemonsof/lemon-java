import sbt._

scalaVersion in Global := "2.10.3"

lazy val compilers = Project(id = "lemon-compilers",base = file("lemon-compilers"))



