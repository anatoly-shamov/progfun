// used for style-checking submissions
libraryDependencies += "org.scalastyle" %% "scalastyle" % "0.8.0"

// used for submitting the assignments to Coursera
libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.2.1"

// used for base64 encoding
libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

// used to escape json for the submission
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4"

// repositories
resolvers += "Maven Central Plain HTTP" at "http://repo1.maven.org/maven2/"
resolvers += "SBP Plugins Plain HTTP" at "http://dl.bintray.com/sbt/sbt-plugin-releases/"