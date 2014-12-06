
name := "Hammurabi"

version := "0.2-SNAPSHOT"

scalaVersion := "2.11.2"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.3.6"
  Seq(
    "org.scala-lang"     %    "scala-reflect" %  "2.11.2",
    "com.typesafe.akka" %% "akka-actor"      % akkaV,
    "com.typesafe.akka" %% "akka-testkit"    % akkaV,
    "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
    "ch.qos.logback"    % "logback-classic"  % "1.1.2",
    "org.scalatest"     % "scalatest_2.11"   % "2.2.1"  % "test",
    "org.mockito"       % "mockito-all"      % "1.9.5"  % "test",
    "junit"             % "junit"            % "4.11"   % "test"
  )
}



