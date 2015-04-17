name := "DotaReplaySummarizer"

version := "1.0"

scalaVersion := "2.11.5"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "com.skadistats" % "clarity" % "1.1"
//  "com.skadistats" % "clarity-protobuf" % "2"
)
