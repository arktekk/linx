import com.typesafe.sbt.pgp.PgpKeys

crossScalaVersions  := Seq("2.12.1", "2.11.8", "2.10.6")
scalaVersion        := crossScalaVersions.value.head
scalacOptions      ++= Seq("-feature", "-deprecation", "-encoding", "utf-8")

//apologies for this hack, it was the only way i could make publishing work for now
isSnapshot in ThisBuild := true

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if ((version in ThisBuild).value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

val noPublishSettings: Project => Project =
  _.settings(
    publish := {},
    publishLocal := {}
  )

val publishSettings: Project => Project =
  _.settings(

    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    pomExtra :=
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
      </developers>
  )

lazy val linx =
  crossProject
    .in(file("."))
    .settings(
      organization        := "no.arktekk",
      name                := "linx",
      description         := "A simple and typesafe link representation",
      homepage            := Some(url("http://github.com/arktekk/linx")),
      licenses            := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % Test
    ).configureAll(publishSettings)

lazy val linxJVM = linx.jvm
lazy val linxJS  = linx.js

lazy val linxRoot = project
  .in(file("root"))
  .aggregate(linxJS, linxJVM)
  .configure(noPublishSettings)
