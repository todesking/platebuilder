scalaVersion := "2.11.8"

lazy val core = project in file("core")

lazy val example = (project in file("example")) dependsOn core

scalariformSettings
