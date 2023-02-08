resolvers += "HMRC-open-artefacts-maven" at "https://open.artefacts.tax.service.gov.uk/maven2"
resolvers += Resolver.url("HMRC-open-artefacts-ivy", url("https://open.artefacts.tax.service.gov.uk/ivy2"))(
  Resolver.ivyStylePatterns
)
resolvers += Resolver.typesafeRepo("releases")

// To resolve a bug with version 2.x.x of the scoverage plugin - https://github.com/sbt/sbt/issues/6997
ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)

addSbtPlugin("uk.gov.hmrc"               % "sbt-auto-build"     % "3.9.0")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"       % "2.4.6")
addSbtPlugin("uk.gov.hmrc"               % "sbt-distributables" % "2.2.0")
addSbtPlugin("com.typesafe.play"         % "sbt-plugin"         % "2.8.18")
addSbtPlugin("com.typesafe.sbt"          % "sbt-gzip"           % "1.0.2")
addSbtPlugin("org.irundaia.sbt"          % "sbt-sassify"        % "1.5.1")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"      % "2.0.5")
addSbtPlugin("ch.epfl.scala"             % "sbt-scalafix"       % "0.9.34")
