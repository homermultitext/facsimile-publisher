

name := "HMT facsimiles"
organization := "org.homermultitext"
version := "0.0.1"
licenses += ("GPL-3.0",url("https://opensource.org/licenses/gpl-3.0.html"))


resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("neelsmith", "maven")

scalaVersion := "2.12.4"
libraryDependencies ++= Seq(

  "edu.holycross.shot" %% "citerelations" % "2.3.0",
  "edu.holycross.shot" %% "cex" % "6.2.1",
  "edu.holycross.shot.cite" %% "xcite" % "3.6.0",
  "edu.holycross.shot" %% "ohco2" % "10.9.2",
  "edu.holycross.shot" %% "dse" % "4.2.0",
  "edu.holycross.shot" %% "scm" % "6.1.3",
  "org.homermultitext" %% "hmt-textmodel" % "4.0.0",
  "org.homermultitext" %% "hmtcexbuilder" % "3.2.1",
  "org.homermultitext" %% "hmt-mom" % "3.5.3",
  "edu.holycross.shot" %% "greek" % "1.4.0",
  "com.github.pathikrit" %% "better-files" % "3.5.0",
)
