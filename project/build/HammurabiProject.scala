/**
 * @author Mario Fusco
 */

import sbt._

class HammurabiProject(info: ProjectInfo) extends DefaultProject(info) {

  val logbackCore = "ch.qos.logback" % "logback-core" % "0.9.22"
  val logbackClassic = "ch.qos.logback" % "logback-classic" % "0.9.22"

  val junit = "junit" % "junit" % "4.7" % "test"
  val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test"
}