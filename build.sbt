
javaOptions in run ++= Seq(
   "-Xms256M",
   "-Xmx4G"
 )


resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("neelsmith", "maven")

scalaVersion := "2.12.4"
libraryDependencies ++= Seq(

  "edu.holycross.shot" %% "citerelations" % "2.3.0",
  "edu.holycross.shot" %% "cex" % "6.2.1",
  "edu.holycross.shot.cite" %% "xcite" % "3.5.0",
  "edu.holycross.shot" %% "ohco2" % "10.9.0",
  "edu.holycross.shot" %% "dse" % "4.2.0",
  "edu.holycross.shot" %% "scm" % "6.1.1",
  "org.homermultitext" %% "hmt-textmodel" % "4.0.0",
  "org.homermultitext" %% "hmtcexbuilder" % "3.2.0",
  "org.homermultitext" %% "hmt-mom" % "3.4.2",
  "edu.holycross.shot" %% "greek" % "1.4.0",
  "com.github.pathikrit" %% "better-files" % "3.5.0",
)
