name := "linx"

organization := "com.jteigen"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.10.0-RC5")

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.10-M2" % "test",
    "junit" % "junit" % "4.11" % "test")

