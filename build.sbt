import com.typesafe.sbt.pgp.PgpKeys

releaseSettings

name := "linx"

organization := "com.jteigen"

scalaVersion := "2.10.0"

description := "A simple and typesafe link representation"

crossScalaVersions := Seq("2.9.1-1", "2.9.2", "2.10.0")

libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.10-M2" % "test",
    "junit" % "junit" % "4.11" % "test")

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publish <<= PgpKeys.publishSigned

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

homepage := Some(url("http://github.com/teigen/linx"))

pomExtra := (
  <scm>
    <url>git@github.com:teigen/linx.git</url>
    <connection>scm:git:git@github.com:teigen/linx.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jteigen</id>
      <name>Jon-Anders Teigen</name>
      <url>http://jteigen.com</url>
    </developer>
  </developers>)
