import sbtassembly.MergeStrategy

val packageVersion = "2.11.1"
val sVersion       = "2.13.6"
val circeVersion   = "0.14.1"

name := "so-testbed"
version := packageVersion
scalacOptions ++= scalac
scalaVersion := sVersion
//javacOptions ++= javac

lazy val routingTestbed = project.in(file(".")).aggregate(core, matsim)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "so-testbed-core",
    version := packageVersion,
    scalaVersion := sVersion,
    scalacOptions ++= scalac,
//    javacOptions ++= javac,
    libraryDependencies ++= coreDependencies ++ loggingDependencies,
    assembly / assemblyMergeStrategy := {
      case "META-INF/io.netty.versions.properties" => MergeStrategy.first
      case x =>
        val oldStrategy: String => MergeStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )
//  .dependsOn(dabtree)

lazy val matsim = project
  .in(file("matsim"))
  .settings(
    name := "so-testbed-matsim",
    version := packageVersion,
    scalaVersion := sVersion,
    scalacOptions ++= scalac,
//    javacOptions ++= javac,
    matsimAssemblyStrategy,
    resolvers ++= matsimResolvers,
    libraryDependencies ++= coreDependencies ++ matsimDependencies ++ loggingDependencies
  )
  .dependsOn(core)

//lazy val coreResolvers = Seq(
//  Resolver.url(
//    "idio",
//    url("http://dl.bintray.com/idio/sbt-plugins")
//  )(Resolver.ivyStylePatterns)
//)

lazy val matsimResolvers = Seq(
  "MATSim release repository".at("https://repo.matsim.org/repository/matsim/"),
  "OSGeo Release Repository".at("https://repo.osgeo.org/repository/release/")
//  "Open Source Geospatial Foundation Repository".at("http://download.osgeo.org/webdav/geotools/"),
//  "OpenGeo Maven Repository".at("http://repo.opengeo.org")
)

//lazy val matsimSpark = project
//  .in(file("matsim-spark"))
//  .settings(
//    name := "so-testbed-matsim-spark",
//    scalaVersion := sVersion,
//    libraryDependencies ++= matsimSparkDependencies
//  )
//  .dependsOn(core, matsim)

lazy val scalac = List(
//  "-target:jvm-1.8",
  "-language:higherKinds", // Cats
//  "-Ypartial-unification",                   // Cats to traverse things like List[Either[A, B]] -> Either[A, List[B]]
  "-Xmacro-settings:materialize-derivations" // better PureConfig error messages
)

//lazy val javac = List("-source", "1.8", "-target", "1.8")

// External Dependencies
lazy val coreDependencies = List(
  // REINFORCEMENT LEARNING LIBRARY
//  "org.deeplearning4j" % "deeplearning4j-core"  % "1.0.0-beta6",
//  "org.nd4j"           % "nd4j-native-platform" % "1.0.0-beta6",
//  "org.deeplearning4j" % "rl4j-api"             % "1.0.0-beta6",
  // PURE FP LIBRARY
//  "org.typelevel" %% "cats-core"   % "2.0.0",
//  "org.typelevel" %% "cats-effect" % "2.0.0",
  "org.typelevel" %% "cats-core"   % "2.3.0",
  "org.typelevel" %% "cats-effect" % "3.2.5",
// COMMAND LINE PARSING
  "com.monovore" %% "decline"        % "2.1.0",
  "com.monovore" %% "decline-effect" % "2.1.0",
  // CONFIG
  "com.github.pureconfig" %% "pureconfig" % "0.14.1",
  // GIS
  "org.locationtech.proj4j" % "proj4j"   % "1.1.1",
  "org.locationtech.jts"    % "jts-core" % "1.17.1",
  "com.uber"                % "h3"       % "3.0.3",
  // XML
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  // CSV
  "com.nrinaudo" %% "kantan.csv" % "0.6.0",
//  "com.nrinaudo" %% "kantan.csv-generic" % "0.6.0",
  // JSON
  "io.circe" %% "circe-core"    % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser"  % circeVersion,
  // MATH
  "org.apache.commons" % "commons-math" % "2.2",
  // TEST
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  // NETWORKING
  "com.softwaremill.sttp.client3" %% "core"                           % "3.3.14",
  "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats" % "3.3.14",
  "com.softwaremill.sttp.client3" %% "circe"                          % "3.3.14",
  "com.google.protobuf"           % "protobuf-java"                   % "3.17.2"
).map(_.exclude("org.slf4j", "*"))

lazy val matsimDependencies = List(
  "org.geotools" % "gt-main" % "21.5",
  "org.matsim"   % "matsim"  % "12.0" // most recent official release/tag
).map(_.exclude("org.slf4j", "*"))

lazy val loggingDependencies = List(
  // LOGGING SYSTEM
  "ch.qos.logback"             % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2"
)

/////////////////////////// sbt-assembly ///////////////////////////

lazy val matsimAssemblyStrategy = Seq(
  Compile / mainClass := Some("edu.colorado.fitzgero.sotestbed.matsim.app.MATSimExperiment2021App"),
  run / mainClass := Some("edu.colorado.fitzgero.sotestbed.matsim.app.MATSimExperiment2021App"),
  packageBin / mainClass := Some("edu.colorado.fitzgero.sotestbed.matsim.app.MATSimExperiment2021App"),
  assembly / mainClass := Some("edu.colorado.fitzgero.sotestbed.matsim.app.MATSimExperiment2021App"),
  assembly / test := {},
  assembly / assemblyMergeStrategy := {

    // early 2020
    case "tec/uom/se/format/messages.properties" => MergeStrategy.concat

    // 20200711 - switched from lib/ matsim  12.0 jar to maven-loaded matsim 12.0
    case "META-INF/sun-jaxb.episode"                         => MergeStrategy.first
    case "META-INF/io.netty.versions.properties"             => MergeStrategy.first
    case PathList(ps @ _*) if ps.last == "Log4j2Plugins.dat" => MergeStrategy.discard
    case PathList("javax", "validation", xs @ _*)            => MergeStrategy.first
    case PathList("javax", "activation", xs @ _*)            => MergeStrategy.first
    case "module-info.class"                                 => MergeStrategy.discard

    // 20210317 - too many logbacks!
    case "logback.xml" => MergeStrategy.first

    case x =>
      val oldStrategy: String => MergeStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  }
)
