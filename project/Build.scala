import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "fbg"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "com.top10" %% "scala-redis-client" % "1.9.0"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    resolvers += "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
  )

}
