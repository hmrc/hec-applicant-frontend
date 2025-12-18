import scoverage.ScoverageKeys

val appName = "hec-applicant-frontend"

lazy val scoverageSettings =
  Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;.*Reverse.*;.*(config|views).*;.*(BuildInfo|Routes).*",
    ScoverageKeys.coverageMinimumStmtTotal := 95.00,
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
    majorVersion := 1,
    scalaVersion := "3.3.6",
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
