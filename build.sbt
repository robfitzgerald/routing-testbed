
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
    libraryDependencies ++= coreDependencies ++ matsimDependencies
  )
  .dependsOn(core)

lazy val scalac =  List(
  "-language:higherKinds"
//  "-Ypartial-unification"
)

// Internal Dependencies
//lazy val dabtree = ProjectRef(uri("https://github.com/robfitzgerald/dabtree.git#master"), "dabtree")
//RootProject(uri("https://github.com/robfitzgerald/dabtree.git"))

// External Dependencies

lazy val coreDependencies = List(
  "org.typelevel" %% "cats-core" % "2.0.0-RC1",
  "org.typelevel" %% "cats-effect" % "1.3.1",
  "com.typesafe.akka" %% "akka-actor" % "2.5.25"
)

lazy val matsimDependencies = List(
  "org.matsim" % "matsim" % "0.10.1"
)