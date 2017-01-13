import com.typesafe.sbt.pgp.PgpKeys

releasePublishArtifactsAction := PgpKeys.publishSigned.value

name := "linx"

organization := "no.arktekk"

description := "A simple and typesafe link representation"

crossScalaVersions := Seq("2.12.1", "2.11.8", "2.10.6")

scalaVersion := crossScalaVersions.value.head

scalacOptions ++= Seq("-feature", "-deprecation", "-encoding", "utf-8")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1"

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if ((version in ThisBuild).value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

homepage := Some(url("http://github.com/arktekk/linx"))

pomExtra := (
  <scm>
    <url>git@github.com:arktekk/linx.git</url>
    <connection>scm:git:git@github.com:arktekk/linx.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jteigen</id>
      <name>Jon-Anders Teigen</name>
      <url>http://jteigen.com</url>
    </developer>
  </developers>)
