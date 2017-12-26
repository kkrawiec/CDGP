
// Uncomment this line to use manually created local clone of the repository
// lazy val fuel = RootProject( file("../fuel") )
lazy val fuel = RootProject(uri("git://github.com/kkrawiec/fuel.git"))

// Uncomment this line to use manually created local clone of the repository
// lazy val swim = RootProject( file("../swim") )
lazy val swim = RootProject(uri("git://github.com/kkrawiec/swim.git"))

// Uncomment this line to use manually created local clone of the repository
// lazy val sygusParser = RootProject( file("../SyGuS") )
lazy val sygusParser = RootProject(uri("git://github.com/JerrySwan/SyGuS.git"))


lazy val root = (project in file(".")).
  settings(
    name := "CDGP",
    version := "1.0",
    mainClass in Compile := Some("app.Main"),
    scalaVersion := "2.11.11",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "junit" % "junit" % "4.12" % Test,
        "com.novocode" % "junit-interface" % "0.11" % Test)
  ).dependsOn(fuel, swim, sygusParser)

