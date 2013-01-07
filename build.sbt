name := "CompactHashTable"

version := "1.0"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation", "-feature", "-optimise", "-Yinline-warnings")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP5"

// only needed by gs-collections which is used for benchmarking
libraryDependencies += "net.jcip" % "jcip-annotations" % "1.0"