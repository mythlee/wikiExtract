lazy val root = (project in file(".")).
    settings(
        name := "wikiExtract",
        organization := "github.com.mythlee",
        version := "0.0.1",
        scalaVersion := "2.11.6"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test" withSources() withJavadoc(),
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test" withSources() withJavadoc()
)

libraryDependencies ++= Seq(
    "edu.emory.mathcs.nlp" % "nlp4j-api" % "1.1.3",
    "edu.emory.mathcs.nlp" % "nlp4j-cli" % "1.1.3",
    "edu.emory.mathcs.nlp" % "nlp4j-english" % "1.1.3"
)

initialCommands := "import github.com.mythlee.wikiextract._"

