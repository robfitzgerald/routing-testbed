
name := "so-testbed"
version := "0.1"
scalaVersion := "2.12.9"

lazy val core = project
  .in(file("core")).
  settings(
    name := "so-testbed-core",
    scalaVersion := "2.12.9",
    scalacOptions ++= scalac,
    libraryDependencies ++= coreDependencies
  )
//  .dependsOn(dabtree)

lazy val matsim = project
  .in(file("matsim")).
  settings(
    name := "so-testbed-matsim",
    scalaVersion := "2.12.9",
    scalacOptions ++= scalac,
    libraryDependencies ++= coreDependencies // ++ matsimDependencies
  )
  .dependsOn(core)

lazy val scalac =  List(
  "-language:higherKinds",                   // FP type wizardry
  "-Xmacro-settings:materialize-derivations" // better PureConfig error messages
//  "-Ypartial-unification"
)

// Internal Dependencies
//lazy val dabtree = ProjectRef(uri("https://github.com/robfitzgerald/dabtree.git#master"), "dabtree")
//RootProject(uri("https://github.com/robfitzgerald/dabtree.git"))

// External Dependencies
lazy val coreDependencies = List(
  // PURE FP LIBRARY
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",

  // LOGGING SYSTEM
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",

  // COMMAND LINE PARSING
  "com.monovore" %% "decline" % "1.0.0",

  // CONFIG
  "com.github.pureconfig" %% "pureconfig" % "0.12.2",
//  "com.typesafe" % "config" % "1.3.4",

  // XML
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",

  // TEST
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

//lazy val matsimDependencies = List(
//  "org.matsim" % "matsim" % "0.10.1" // most recent official release/tag
//)
//lazy val matsimGithubDependency = ProjectRef(uri("https://github.com/matsim-org/matsim.git#master"), "matsim")