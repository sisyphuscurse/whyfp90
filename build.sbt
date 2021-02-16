name := "fpmatters"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies += "org.vegas-viz" % "vegas_2.11" % "0.3.11"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"

libraryDependencies += "org.typelevel" %% "cats-mtl-core" % "0.4.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.1.0"

scalacOptions += "-Ypartial-unification"
/*
libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case "2.11" ⇒ "1.6.2"
    case _ => "1.1.0-34-b991be4"
  }
  "com.lihaoyi" % "ammonite_2.12" % version % "test" cross CrossVersion.full
}
sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

// Optional, required for the `source` command to work
(fullClasspath in Test) ++= {
  (updateClassifiers in Test).value
    .configurations
    .find(_.configuration == Test.name)
    .get
    .modules
    .flatMap(_.artifacts)
    .collect{case (a, f) if a.classifier == Some("sources") => f}
}
*/
