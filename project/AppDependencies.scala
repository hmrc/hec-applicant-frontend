import play.core.PlayVersion
import sbt._

object AppDependencies {

  val compile = Seq(
    "uk.gov.hmrc"                %% "bootstrap-frontend-play-28" % "5.3.0",
    "uk.gov.hmrc"                %% "play-frontend-hmrc"         % "0.89.0-play-28",
    "uk.gov.hmrc"                %% "mongo-caching"              % "7.0.0-play-28",
    "uk.gov.hmrc"                %% "domain"                     % "6.2.0-play-28",
    "org.typelevel"              %% "cats-core"                  % "2.1.0",
    "ai.x"                       %% "play-json-extensions"       % "0.42.0",
    "com.github.kxbmap"          %% "configs"                    % "0.4.4",
    "com.github.julien-truffaut" %% "monocle-core"               % "2.0.4",
    "com.github.julien-truffaut" %% "monocle-macro"              % "2.0.4"
  )

  val test = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-28" % "5.3.0"             % Test,
    "org.scalatest"          %% "scalatest"              % "3.2.5"             % Test,
    "org.jsoup"               % "jsoup"                  % "1.13.1"            % Test,
    "com.typesafe.play"      %% "play-test"              % PlayVersion.current % Test,
    "org.scalamock"          %% "scalamock"              % "4.2.0"             % Test,
    "uk.gov.hmrc"            %% "reactivemongo-test"     % "5.0.0-play-28"     % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"     % "5.1.0"             % "test, it",
    "com.vladsch.flexmark"    % "flexmark-all"           % "0.36.8"            % "test, it"
  )
}
