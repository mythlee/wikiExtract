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

libraryDependencies ++= Seq(
   "org.backuity.clist" %% "clist-core"   % "3.2.2",
   "org.backuity.clist" %% "clist-macros" % "3.2.2" % "provided")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-compress" % "1.12",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.2"
)

initialCommands := "import github.com.mythlee.wikiextract._"


