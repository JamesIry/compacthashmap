import sbt._
import Keys._

object Build extends Build {

  lazy val project = Project("root", file(".")).settings(

    // basics
    name := "CompactHashTable",
    organization := "com.typesafe",
    version := "1.0.0-SNAPSHOT",
    scalaVersion := "2.10.0",

    scalacOptions ++= Seq("-deprecation", "-feature", "-optimise", "-Yinline-warnings"),

    // dependencies
    libraryDependencies ++= Seq(
        "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
        "com.google.code.gson" % "gson" % "1.7.1",
        "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP5",
        // only needed by gs-collections which is used for benchmarking
        "net.jcip" % "jcip-annotations" % "1.0",
        "com.google.caliper" % "caliper" % "0.5-rc1"
    ),
    resolvers += "sonatypeSnapshots" at "http://oss.sonatype.org/content/repositories/snapshots",

    // enable forking in run
    fork in run := true,

    // we need to add the runtime classpath as a "-cp" argument to the `javaOptions in run`, otherwise caliper
    // will not see the right classpath and die with a ConfigurationException
    javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Build.data(cp).mkString(":")) }
  )
}
