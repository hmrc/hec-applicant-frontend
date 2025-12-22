import scoverage.ScoverageKeys
import uk.gov.hmrc.DefaultBuildSettings.itSettings

val appName = "hec-applicant-frontend"

ThisBuild / scalaVersion := "3.3.6"
ThisBuild / majorVersion := 1

lazy val it = project
  .enablePlugins(PlayScala)
  .dependsOn(microservice % "test->test") // the "test->test" allows reusing test code and test dependencies
  .settings(itSettings())
  .settings(libraryDependencies ++= AppDependencies.itDependencies)

lazy val scoverageSettings =
  Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*Reverse.*;.*(config|views).*;.*(BuildInfo|Routes).*",
    ScoverageKeys.coverageMinimumStmtTotal := 90.00,
    ScoverageKeys.coverageFailOnMinimum := true,
    ScoverageKeys.coverageHighlighting := true
  )

lazy val microservice = Project(appName, file("."))
  .enablePlugins(
    play.sbt.PlayScala,
    SbtDistributablesPlugin
  )
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(
    // To resolve a bug with version 2.x.x of the scoverage plugin - https://github.com/sbt/sbt/issues/6997
    libraryDependencySchemes ++= Seq("org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always)
  )
  .settings(
    libraryDependencies ++= AppDependencies.compile ++ AppDependencies.test
  )
  .settings(
    scalacOptions += "-Wconf:src=routes/.*:s",
    scalacOptions += "-Wconf:msg=unused.import&src=html/.*:s",
    scalacOptions += "-Wconf:msg=unused.explicit.parameter&src=html/.*:s",
    scalacOptions += "-Wconf:msg=unused.import&src=xml/.*:s",
    scalacOptions += "-Wconf:msg=Flag.*repeatedly:s"
  )
  .settings(routesImport := Seq.empty)
  .settings(TwirlKeys.templateImports := Seq.empty)
  .settings(scoverageSettings: _*)
  .settings(scalafmtOnCompile := true)
  .settings(PlayKeys.playDefaultPort := 10106)
