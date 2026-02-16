import sbt.*

object AppDependencies {

  val bootStrapVersion = "10.5.0"
  val monocleVersion   = "3.3.0"
  val hmrcMongoVersion = "2.12.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-30"                 % bootStrapVersion,
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-play-30"                         % hmrcMongoVersion,
    "uk.gov.hmrc"                %% "domain-play-30"                             % "13.0.0",
    "org.typelevel"              %% "cats-core"                                  % "2.13.0",
    "dev.optics"                 %% "monocle-core"                               % monocleVersion,
    "dev.optics"                 %% "monocle-macro"                              % monocleVersion,
    "uk.gov.hmrc"                %% "play-conditional-form-mapping-play-30"      % "3.4.0",
    "uk.gov.hmrc"                %% "play-frontend-hmrc-play-30"                 % "12.31.0",
    "org.apache.commons"          % "commons-lang3"                              % "3.18.0",
    "ch.qos.logback"              % "logback-core"                               % "1.5.27",
    "ch.qos.logback"              % "logback-classic"                            % "1.5.27",
    "at.yawk.lz4"                 %  "lz4-java"                                  % "1.10.3"
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"       %% "bootstrap-test-play-30"  % bootStrapVersion % Test,
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-test-play-30" % hmrcMongoVersion % Test,
    "org.scalamock"     %% "scalamock"               % "7.5.0"          % Test
  )

  val itDependencies: Seq[ModuleID] = Seq(
    "uk.gov.hmrc" %% "bootstrap-test-play-30" % bootStrapVersion % Test
  )

}
