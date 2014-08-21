resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
