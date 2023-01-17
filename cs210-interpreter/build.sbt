course := "progfun2"
assignment := "interpreter"

scalaVersion := "3.2.0"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies += "org.scalameta" %% "munit" % "0.7.26" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"
