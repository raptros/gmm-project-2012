resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.5")

addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.6.0")

//addSbtPlugin("com.typesafe.startscript" % "xsbt-start-script-plugin" % "0.5.2")
