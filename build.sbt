name := "Sudoku Solver"
version := "0.1"
scalaVersion := "2.11.7"

//http4s
libraryDependencies += "org.http4s" %% "http4s-dsl"          % "latest.integration"  // to use the core dsl
libraryDependencies += "org.http4s" %% "http4s-blaze-server" % "latest.integration"  // to use the blaze backend
libraryDependencies += "org.http4s" %% "http4s-servlet"      % "latest.integration"  // to use the raw servlet backend
libraryDependencies += "org.http4s" %% "http4s-jetty"        % "latest.integration"  // to use the jetty servlet backend
libraryDependencies += "org.http4s" %% "http4s-blaze-client" % "latest.integration"  // to use the blaze client

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
