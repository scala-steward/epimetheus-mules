import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val `epimetheus-mules` = project.in(file("."))
  .disablePlugins(MimaPlugin)
  .settings(skip in publish := true)
  .settings(commonSettings)
  .aggregate(core, site)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "epimetheus-mules"
  )

lazy val site = project.in(file("site"))
  .disablePlugins(MimaPlugin)
  .settings(skip in publish := true)
  .settings(commonSettings)
  .dependsOn(core)
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(TutPlugin)
  .settings{
    import microsites._
    Seq(
      micrositeName := "epimetheus-mules",
      micrositeDescription := "Metrics for Mules Caches",
      micrositeAuthor := "Christopher Davenport",
      micrositeGithubOwner := "ChristopherDavenport",
      micrositeGithubRepo := "epimetheus-mules",
      micrositeBaseUrl := "/epimetheus-mules",
      micrositeDocumentationUrl := "https://www.javadoc.io/doc/io.chrisdavenport/epimetheus-mules_2.12",
      micrositeFooterText := None,
      micrositeHighlightTheme := "atom-one-light",
      micrositePalette := Map(
        "brand-primary" -> "#3e5b95",
        "brand-secondary" -> "#294066",
        "brand-tertiary" -> "#2d5799",
        "gray-dark" -> "#49494B",
        "gray" -> "#7B7B7E",
        "gray-light" -> "#E5E5E6",
        "gray-lighter" -> "#F4F3F4",
        "white-color" -> "#FFFFFF"
      ),
      fork in tut := true,
      scalacOptions in Tut --= Seq(
        "-Xfatal-warnings",
        "-Ywarn-unused-import",
        "-Ywarn-numeric-widen",
        "-Ywarn-dead-code",
        "-Ywarn-unused:imports",
        "-Xlint:-missing-interpolator,_"
      ),
      micrositePushSiteWith := GitHub4s,
      micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
      micrositeExtraMdFiles := Map(
          file("CHANGELOG.md")        -> ExtraMdFileConfig("changelog.md", "page", Map("title" -> "changelog", "section" -> "changelog", "position" -> "100")),
          file("CODE_OF_CONDUCT.md")  -> ExtraMdFileConfig("code-of-conduct.md",   "page", Map("title" -> "code of conduct",   "section" -> "code of conduct",   "position" -> "101")),
          file("LICENSE")             -> ExtraMdFileConfig("license.md",   "page", Map("title" -> "license",   "section" -> "license",   "position" -> "102"))
      )
    )
  }

val catsV = "2.1.1"
val catsEffectV = "2.1.4"
val epimetheusV = "0.4.0"
val mulesV = "0.4.0"

val specs2V = "4.8.3"

val kindProjectorV = "0.11.3"
val betterMonadicForV = "0.3.1"

// General Settings
lazy val commonSettings = Seq(
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.10"),

  addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorV cross CrossVersion.full),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForV),
  libraryDependencies ++= Seq(
    "org.typelevel"               %% "cats-core"                  % catsV,
    "org.typelevel"               %% "cats-effect"                % catsEffectV,
    "io.chrisdavenport"           %% "epimetheus"                 % epimetheusV,
    "io.chrisdavenport"           %% "mules"                      % mulesV,

    "org.specs2"                  %% "specs2-core"                % specs2V       % Test,
    "org.specs2"                  %% "specs2-scalacheck"          % specs2V       % Test
  )
)

inThisBuild(List(
  organization := "io.chrisdavenport",
  homepage := Some(url("https://github.com/ChristopherDavenport/epimetheus-mules")),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "ChristopherDavenport",
      "Christopher Davenport",
      "chris@christopherdavenport.tech",
      url("https://github.com/ChristopherDavenport")
    )
  ),
  scalacOptions in (Compile, doc) ++= Seq(
      "-groups",
      "-sourcepath", (baseDirectory in LocalRootProject).value.getAbsolutePath,
      "-doc-source-url", "https://github.com/ChristopherDavenport/epimetheus-mules/blob/v" + version.value + "€{FILE_PATH}.scala"
  ),
))
