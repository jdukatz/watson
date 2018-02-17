name := "watson"

version := "1.0-SNAPSHOT"

organization := "edu.arizona.cs"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies ++= Seq(
	"org.apache.lucene" % "lucene-analyzers-common" % "6.4.1",
	"org.apache.lucene" % "lucene-queryparser" % "6.4.1",
	"edu.stanford.nlp" % "stanford-corenlp" % "3.4",
	"edu.stanford.nlp" % "stanford-corenlp" % "3.4" classifier "models",
	"edu.stanford.nlp" % "stanford-parser" % "3.4"
	)