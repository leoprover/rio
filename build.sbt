lazy val rio = (project in file("."))
  .settings(
    name := "rio",
    version := "1.4",
    description := "A reasoner for Input/Output logics",
    organization := "net.aurelee",
    scalaVersion := "2.13.18",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
    ),
    Compile/unmanagedResourceDirectories += baseDirectory.value / "contrib",
    Compile/mainClass  := Some("net.aurelee.rio.Main"),
    assembly/mainClass := Some("net.aurelee.rio.Main"),
    assembly/assemblyJarName := s"${name.value}-${version.value}.jar",

    licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    libraryDependencies += "net.java.dev.jna" % "jna" % "5.18.1",
    libraryDependencies += "io.github.leoprover" %% "scala-tptp-parser" % "1.7.3",
    
    nativeImageOptions += s"-H:+UnlockExperimentalVMOptions",
    nativeImageOptions += s"-H:ReflectionConfigurationFiles=${baseDirectory.value / "contrib" / "native-image-configs" / "reflect-config.json"}",
    nativeImageOptions += s"-H:ConfigurationFileDirectories=${baseDirectory.value / "contrib" / "native-image-configs" }",
    nativeImageOptions +="-H:+JNI",
    nativeImageOptions += s"--future-defaults=all",
    nativeImageOptions += s"-R:MaxHeapSize=512m",
    nativeImageOptions += s"-march=native",
  ).enablePlugins(NativeImagePlugin)
