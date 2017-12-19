name := course.value + "-" + assignment.value

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation")

courseId := "GVy8tIIKEeWXmQ4F86nmrw"

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % Test
libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.6",
  "com.github.scala-blitz" %% "scala-blitz" % "1.1",
  "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
  "com.storm-enroute" %% "scalameter" % "0.6" % Test
)

// repositories
resolvers += "Maven Central Plain HTTP" at "http://repo1.maven.org/maven2/"

// include the common dir
commonSourcePackages += "common"
