organization in Global := "lemon"

scalaVersion in Global := "2.10.3"

resolvers in Global += "easytaxi" at "http://10.143.132.78:8081/artifactory/repo/"

libraryDependencies in Global += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies in Global += "org.slf4j" % "slf4j-api" % "1.7.5"

libraryDependencies in Global += "ch.qos.logback" % "logback-core" % "1.0.13"

libraryDependencies in Global += "ch.qos.logback" % "logback-classic" % "1.0.13"

libraryDependencies in Global += "junit" % "junit" % "4.10" % "test"

libraryDependencies in Global += "cglib" % "cglib" % "3.1"