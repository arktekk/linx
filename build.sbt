name := "linx"

organization := "com.jteigen"

scalaVersion := "2.10.0"

description := "A library for simple link and path representations"

crossScalaVersions := Seq("2.8.0", "2.8.1", "2.8.2", "2.9.0" ,"2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.10.0")

version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.10-M2" % "test",
    "junit" % "junit" % "4.11" % "test")

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
