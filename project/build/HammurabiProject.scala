/**
 * @author Mario Fusco
 */

import sbt._

class HammurabiProject(info: ProjectInfo) extends DefaultProject(info) {

  val junit = "junit" % "junit" % "4.7" % "test"
  val scalatest = "org.scalatest" % "scalatest" % "1.3" % "test"

}