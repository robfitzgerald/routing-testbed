import sbtassembly.MergeStrategy

name := "so-testbed"
val packageVersion = "2.1.4"
version := packageVersion
val sVersion = "2.12.10"

lazy val core = project
  .in(file("core"))
  .settings(
    name := "so-testbed-core",
    scalaVersion := sVersion,
    scalacOptions ++= scalac,
    libraryDependencies ++= coreDependencies
  )
//  .dependsOn(dabtree)

lazy val matsim = project
  .in(file("matsim"))
  .settings(
    name := "so-testbed-matsim",
    version := packageVersion,
    scalaVersion := sVersion,
    scalacOptions ++= scalac,
    matsimAssemblyStrategy,
    resolvers ++= matsimRepository,
    libraryDependencies ++= coreDependencies ++ matsimDependencies
  )
  .dependsOn(core)

lazy val matsimRepository = Seq(
  "MATSim release repository".at("http://dl.bintray.com/matsim/matsim"),
  "OSGeo Release Repository".at("https://repo.osgeo.org/repository/release/"),
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
  "-language:higherKinds", // FP type wizardry
  "-Xmacro-settings:materialize-derivations" // better PureConfig error messages
//  "-Ypartial-unification"
)

// External Dependencies
lazy val coreDependencies = List(
  // REINFORCEMENT LEARNING LIBRARY
//  "org.deeplearning4j" % "deeplearning4j-core"  % "1.0.0-beta6",
//  "org.nd4j"           % "nd4j-native-platform" % "1.0.0-beta6",
//  "org.deeplearning4j" % "rl4j-api"             % "1.0.0-beta6",
  // PURE FP LIBRARY
  "org.typelevel" %% "cats-core"   % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  // LOGGING SYSTEM
  "ch.qos.logback"             % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2",
  // COMMAND LINE PARSING
  "com.monovore" %% "decline" % "1.0.0",
  // CONFIG
  "com.github.pureconfig" %% "pureconfig" % "0.12.2",
//  "com.typesafe" % "config" % "1.3.4",
  // GIS
  "org.locationtech.proj4j" % "proj4j" % "1.1.1",
  "com.uber"                % "h3"     % "3.0.3",
//  "org.locationtech.jts" % "jts-core" % "1.16.1",
  // XML
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  // CSV
  "com.nrinaudo" %% "kantan.csv" % "0.6.0",
//  "com.nrinaudo" %% "kantan.csv-generic" % "0.6.0",
// TEST
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

//lazy val matsimSparkDependencies = List(
//  "org.apache.spark" %% "spark-sql" % "2.4.4" % "provided"
//)

lazy val matsimDependencies = List(
  "org.geotools" % "gt-main" % "21.5",
  "org.matsim"   % "matsim"  % "12.0" // most recent official release/tag
)
//lazy val matsimGithubDependency = ProjectRef(uri("https://github.com/matsim-org/matsim.git#master"), "matsim")

/////////////////////////// sbt-assembly ///////////////////////////

lazy val matsimAssemblyStrategy = Seq(
  mainClass in (Compile, run) := Some("edu.colorado.fitzgero.sotestbed.matsim.app.MATSimBatchExperimentApp"),
  mainClass in (Compile, packageBin) := Some("edu.colorado.fitzgero.sotestbed.matsim.app.MATSimBatchExperimentApp"),
  mainClass in assembly := Some("edu.colorado.fitzgero.sotestbed.matsim.app.MATSimBatchExperimentApp"),
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
//    case "META-INF/sun-jaxb.episode"                   => MergeStrategy.concat
//    case "META-INF/sun-jaxb.episode" => MergeStrategy.discard
//    case "messages.properties" => MergeStrategy.concat
    case "tec/uom/se/format/messages.properties" => MergeStrategy.concat
    case x =>
      val oldStrategy: String => MergeStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)
