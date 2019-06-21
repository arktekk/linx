import com.typesafe.sbt.pgp.PgpKeys
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

crossScalaVersions  := Seq("2.13.0", "2.12.8", "2.11.12")
scalaVersion        := crossScalaVersions.value.head
scalacOptions      ++= Seq("-feature", "-deprecation", "-encoding", "utf-8")

//apologies for this hack, it was the only way i could make publishing work for now
isSnapshot in ThisBuild := true

publishTo in ThisBuild := {
  if ((isSnapshot in ThisBuild).value)
    Some(Opts.resolver.sonatypeSnapshots)
  else
    Some(Opts.resolver.sonatypeStaging)
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
    scmInfo := Some(ScmInfo(
      new URL("https://github.com/arktekk/linx"),
      "scm:git:git@github.com:arktekk/linx.git"
    )),
    developers += Developer(
      "jteigen",
      "Jon-Anders Teigen",
      null,
      new URL("http://jteigen.com")
    )
  )

lazy val linx =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .in(file("."))
    .settings(
      organization        := "no.arktekk",
      name                := "linx",
      description         := "A simple and typesafe link representation",
      homepage            := Some(url("http://github.com/arktekk/linx")),
      licenses            := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
      libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % Test
    ).configure(publishSettings)

lazy val linxJVM = linx.jvm
lazy val linxJS  = linx.js

lazy val linxRoot = project
  .in(file("root"))
  .aggregate(linxJS, linxJVM)
  .configure(noPublishSettings)
