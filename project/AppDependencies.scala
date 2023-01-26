import play.core.PlayVersion
import sbt._

object AppDependencies {

  val bootStrapVersion = "7.12.0"
  val monocleVersion   = "2.1.0"
  val hmrcMongoVersion = "0.74.0"

  val compile = Seq(
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-28"    % bootStrapVersion,
    "uk.gov.hmrc.mongo"          %% "hmrc-mongo-play-28"            % hmrcMongoVersion,
    "uk.gov.hmrc"                %% "play-frontend-hmrc"            % "6.2.0-play-28",
    "uk.gov.hmrc"                %% "domain"                        % "8.1.0-play-28",
    "org.typelevel"              %% "cats-core"                     % "2.8.0",
    "ai.x"                       %% "play-json-extensions"          % "0.42.0",
    "com.github.kxbmap"          %% "configs"                       % "0.6.1",
    "com.github.julien-truffaut" %% "monocle-core"                  % monocleVersion,
    "com.github.julien-truffaut" %% "monocle-macro"                 % monocleVersion,
    "uk.gov.hmrc"                %% "emailaddress"                  % "3.7.0",
    "uk.gov.hmrc"                %% "play-conditional-form-mapping" % "1.12.0-play-28"
  )

  val test = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-28"  % bootStrapVersion    % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-28" % hmrcMongoVersion    % Test,
    "org.scalatest"          %% "scalatest"               % "3.2.12"            % Test,
    "org.jsoup"               % "jsoup"                   % "1.15.2"            % Test,
    "com.typesafe.play"      %% "play-test"               % PlayVersion.current % Test,
    "org.scalamock"          %% "scalamock"               % "5.2.0"             % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "5.1.0"             % "test, it",
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.62.2"            % "test, it"
  )
}
