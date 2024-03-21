import play.core.PlayVersion
import sbt._

object AppDependencies {

  val bootStrapVersion = "8.4.0"
  val monocleVersion   = "2.1.0"
  val hmrcMongoVersion = "1.7.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-30"              % bootStrapVersion,
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-play-30"                      % hmrcMongoVersion,
    "uk.gov.hmrc"                %% "play-frontend-hmrc-play-30"              % "8.5.0",
    "uk.gov.hmrc"                %% "domain-play-30"                          % "9.0.0",
    "org.typelevel"              %% "cats-core"                               % "2.10.0",
    "com.github.julien-truffaut" %% "monocle-core"                            % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-macro"                           % monocleVersion,
    "uk.gov.hmrc"                %% "emailaddress"                            % "3.8.0",
    "uk.gov.hmrc"                %% "play-conditional-form-mapping-play-30"   % "2.0.0"
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-30"  % bootStrapVersion    % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-30" % hmrcMongoVersion    % Test,
    "org.scalamock"          %% "scalamock"               % "5.2.0"             % Test
  )
}
