resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
