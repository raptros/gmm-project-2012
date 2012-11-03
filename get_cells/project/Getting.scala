import sbt._
import Keys._
/*
object GettingBuild extends Build {
  val logFilePath = SettingKey[File]("log-file", "where the log file to parse is")
  val coordsFilePath = SettingKey[File]("coords-file", "where the wiki coords file is")
  val storeFilePath = SettingKey[File]("store-file", "where to store output")

  val getCells = TaskKey[Unit]("get-cells", "gets cells")

  [>override lazy val settings = super.settings ++
  Seq(sampleKeyA := "A: in Build.settings in Build.scala", resolvers := Seq())
  <]
  lazy val root = Project(id = "hello",
    base = file("."),
    settings = Project.defaultSettings)
}
*/
