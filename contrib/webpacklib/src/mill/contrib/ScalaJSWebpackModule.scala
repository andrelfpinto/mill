import java.io._
import java.util.zip.{ZipInputStream, ZipEntry}

import mill._
import mill.define.{Target, Task}
import mill.scalajslib._

trait ScalaJSDepsModule extends ScalaJSModule {
  case class JsDeps(dependencies: List[(String, String)] = Nil,
                    devDependencies: List[(String, String)] = Nil,
                    jsSources: Map[String, String] = Map.empty) {
    def ++(that: JsDeps): JsDeps =
      JsDeps(
        dependencies ++ that.dependencies,
        devDependencies ++ that.devDependencies,
        jsSources ++ that.jsSources)
  }

  object JsDeps {
    def apply(dependencies: (String, String)*): JsDeps = JsDeps(dependencies = dependencies.toList)
    implicit def rw: upickle.default.ReadWriter[JsDeps] = upickle.default.macroRW
  }

  def moduleDepJsDepsTarget =
    Task.sequence(recursiveModuleDeps.collect { case mod: ScalaJSDepsModule => mod.jsDeps })

  def jsDeps: Target[JsDeps] = T {
    val jsDepsFromIvyDeps =
      resolveDeps(transitiveIvyDeps)().toList.flatMap(pathRef => jsDepsFromJar(pathRef.path.toIO))
    val allJsDeps = jsDepsFromIvyDeps ++ moduleDepJsDepsTarget()
    allJsDeps.foldLeft(JsDeps())(_ ++ _)
  }

  private def jsDepsFromJar(jar: File): Seq[JsDeps] = {
    val npmDependency: PartialFunction[ZipEntry, JsDeps] = {
      case e if e.getName == "NPM_DEPENDENCIES" =>
        val contentsAsJson = ujson.read(readAllBytes(stream)).obj

        def dependenciesOfType(key: String): List[(String, String)] =
          contentsAsJson.getOrElse(key, ujson.Arr()).arr.flatMap(_.obj.map { case (s, v) => s -> v.str }).toList

        JsDeps(
          dependenciesOfType("compileDependencies") ++ dependenciesOfType("compile-dependencies"),
          dependenciesOfType("compileDevDependencies") ++ dependenciesOfType("compile-devDependencies")
        )
    }

    val jsSources: PartialFunction[ZipEntry, JsDeps] = {
      case e if e.getName.endsWith(".js") && !e.getName.startsWith("scala/") =>
        JsDeps(Nil, Nil, Map(e.getName -> readAllBytes(stream)))
    }

    val stream = new ZipInputStream(new BufferedInputStream(new FileInputStream(jar)))

    try {
      Iterator.continually(stream.getNextEntry)
        .takeWhile(_ != null)
        .collect(npmDependency orElse jsSources)
        .toList
    } finally {
      stream.close()
    }
  }

  private def readAllBytes(in: InputStream): String = {
    val bytes = Iterator.continually(in.read).takeWhile(_ != -1).map(_.toByte).toArray
    new String(bytes)
  }
}

trait ScalaJSWebpackBaseModule extends ScalaJSDepsModule {
  def webpackVersion: Target[String] = "4.17.1"
  def webpackCliVersion: Target[String] = "3.1.0"
  def webpackDevServerVersion: Target[String] = "3.1.7"

  case class WebpackParams(inputFile: os.Path,
                           jsDeps: JsDeps,
                           outputDirectory: os.Path,
                           opt: Boolean,
                           libraryName: Option[String]) {
    lazy val copiedInputFile = outputDirectory / inputFile.last
  }

  def writePackageJson = T.task { params: WebpackParams =>
    val webpackDevDependencies = Seq(
      "webpack" -> webpackVersion(),
      "webpack-cli" -> webpackCliVersion(),
      "webpack-dev-server" -> webpackDevServerVersion(),
      "source-map-loader" -> "0.2.3"
    )
    os.write.over(
      params.outputDirectory / "package.json",
      ujson.Obj(
        "dependencies" -> deps.dependencies,
        "devDependencies" -> (deps.devDependencies ++ webpackDevDependencies)
      ).render(2) + "\n"
    )
  }


  def bundleFilename = T {
    "out-bundle.js"
  }

  def webpack = T.task { params: WebpackParams =>
    val _bundleFilename = bundleFilename()
    if (params.inputFile != params.copiedInputFile)
      os.copy.over(params.inputFile, params.copiedInputFile)
    params.jsDeps.jsSources foreach { case (n, s) => os.write(params.outputDirectory / n, s) }
    writeWpConfig(params, _bundleFilename)
    writePackageJson().apply(params)
    val logger = T.ctx().log
    val npmInstall = os.proc("npm", "install").call(params.outputDirectory)
    logger.debug(npmInstall.out.string)
    val webpackPath = params.outputDirectory / "node_modules" / "webpack" / "bin" / "webpack"
    val webpack =
      os.proc("node", webpackPath, "--bail", "--profile", "--config", webpackConfigFilename)
        .call(params.outputDirectory)
    logger.debug(webpack.out.string)
    if (params.inputFile != params.copiedInputFile)
    os.remove(params.copiedInputFile)
    List(
      PathRef(params.outputDirectory / _bundleFilename),
      PathRef(params.outputDirectory / (_bundleFilename + ".map"))
    )
  }

  val webpackConfigFilename = "webpack.config.js"

  def writeWpConfig(params: WebpackParams, bundleFilename: String) = {
    val libraryOutputCfg =
      params.libraryName.map(n => Map("library" -> n, "libraryTarget" -> "var")).getOrElse(Map.empty)
    val outputCfg =
      libraryOutputCfg ++ Map("path" -> params.outputDirectory.toString, "filename" -> bundleFilename)
    os.write.over(
      params.outputDirectory / webpackConfigFilename,
      "module.exports = " + ujson.Obj(
        "mode" -> (if (params.opt) "production" else "development"),
        "devtool" -> "source-map",
        "entry" -> params.copiedInputFile.toString,
        "output" -> ujson.Obj.from(outputCfg.mapValues(ujson.Str))
      ).render(2) + ";\n"
    )
  }

  def devWebpack: Target[Seq[PathRef]]
  def prodWebpack: Target[Seq[PathRef]]
}

trait ScalaJSWebpackApplicationModule extends ScalaJSWebpackBaseModule {
  override def devWebpack: Target[Seq[PathRef]] = T.persistent {
    webpack().apply(WebpackParams(fastOpt().path, jsDeps(), T.ctx().dest, opt = false, None))
  }

  override def prodWebpack: Target[Seq[PathRef]] = T.persistent {
    webpack().apply(WebpackParams(fullOpt().path, jsDeps(), T.ctx().dest, opt = true, None))
  }
}

trait ScalaJSWebpackLibraryModule extends ScalaJSWebpackBaseModule {
  def writeEntrypoint = T.task { (dest: os.Path, jsDeps: JsDeps) =>
    val depNames = jsDeps.dependencies.map(_._1).toSet
    val path = dest / "entrypoint.js"
    os.write.over(
      path,
      s"""
         |module.exports = {
         |  "require": (function(moduleName) {
         |    return {
         |      ${depNames.map { name => s"'$name': require('$name')" }.mkString(",\n      ")}
         |    }[moduleName]
         |  })
         |}
         |""".stripMargin
        .trim
    )
    PathRef(path)
  }

  def webpackLibraryName = T {
    "app"
  }

  override def devWebpack: Target[Seq[PathRef]] = T.persistent {
    val dest = T.ctx().dest
    val deps = jsDeps()
    val entrypoint = writeEntrypoint().apply(dest, deps).path
    webpack().apply(WebpackParams(entrypoint, deps, dest, opt = false, Some(webpackLibraryName()))) :+
      fastOpt()
  }

  override def prodWebpack: Target[Seq[PathRef]] = T.persistent {
    val dest = T.ctx().dest
    val deps = jsDeps()
    val entrypoint = writeEntrypoint().apply(dest, deps).path
    webpack().apply(WebpackParams(entrypoint, deps, dest, opt = true, Some(webpackLibraryName()))) :+
      fullOpt()
  }
}
