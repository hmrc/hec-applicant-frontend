import sbt.*

object AppDependencies {

  val bootStrapVersion = "9.18.0"
  val monocleVersion   = "2.1.0"
  val hmrcMongoVersion = "2.6.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-30"                 % bootStrapVersion,
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-play-30"                         % hmrcMongoVersion,
    "uk.gov.hmrc"                %% "domain-play-30"                             % "10.0.0",
    "org.typelevel"              %% "cats-core"                                  % "2.13.0",
    "com.github.julien-truffaut" %% "monocle-core"                               % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-macro"                              % monocleVersion,
    "uk.gov.hmrc"                %% "play-conditional-form-mapping-play-30"      % "3.3.0",
    "uk.gov.hmrc"                %% "play-frontend-hmrc-play-30"                 % "12.7.0"
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"       %% "bootstrap-test-play-30"  % bootStrapVersion % Test,
    "uk.gov.hmrc.mongo" %% "hmrc-mongo-test-play-30" % hmrcMongoVersion % Test,
    "org.scalamock"     %% "scalamock"               % "5.2.0"          % Test
  )
}
