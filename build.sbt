import sbtassembly.MergeStrategy

name := "so-testbed"
val packageVersion = "2.1.0"
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
    libraryDependencies ++= coreDependencies // ++ matsimDependencies
  )
  .dependsOn(core)

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

// Internal Dependencies
//lazy val dabtree = ProjectRef(uri("https://github.com/robfitzgerald/dabtree.git#master"), "dabtree")
//RootProject(uri("https://github.com/robfitzgerald/dabtree.git"))

// External Dependencies
lazy val coreDependencies = List(
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
  // GEOMETRY
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

//lazy val matsimDependencies = List(
//  "org.matsim" % "matsim" % "0.10.1" // most recent official release/tag
//)
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
