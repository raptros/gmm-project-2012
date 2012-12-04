import com.typesafe.sbt.SbtStartScript

//SbtStartScript.StartScriptKeys.startScriptFile <<= (target) { (target) => target / "dostart" }

seq(SbtStartScript.startScriptForJarSettings: _*)

name := "get_cells"

version := "0.1"

//libraryDependencies += "OpenNLP" % "textgrounder" % "0.1.0"

resolvers += "Virtual-Void repository" at "http://mvn.virtual-void.net"

addCompilerPlugin("net.virtualvoid" % "scala-enhanced-strings_2.9.1" % "0.5.2")

