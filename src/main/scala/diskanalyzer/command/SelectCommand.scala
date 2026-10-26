package diskanalyzer.command

import java.nio.file.*
import scala.io.StdIn
import diskanalyzer.app.AppState
import diskanalyzer.Logger


object SelectCommand extends Command:
  val label = "выбрать другой каталог/диск"

  def execute(state: AppState): Unit =
    val raw = StdIn.readLine("Введите путь к каталогу или диску: ")
    if raw == null then
      println("Ввод завершён — выход из программы.")
      Logger.info("Ввод завершён при выборе каталога")
      state.exitRequested = true
    else if raw.isBlank then
      println("Путь не может быть пустым.")
      Logger.warn("Некорректные параметры: пустой путь при выборе каталога")
    else
      try
        val path = Paths.get(raw.trim.stripPrefix("\"").stripSuffix("\"")).toAbsolutePath.normalize()
        if !Files.exists(path) then
          println(s"Путь не существует: $path")
          Logger.warn("Некорректные параметры: указанный путь не существует")
        else if !Files.isDirectory(path) then
          println(s"Указанный путь не является каталогом: $path")
          Logger.warn("Некорректные параметры: путь не является каталогом")
        else
          println(s"Выбран каталог: $path")
          Logger.info(s"Выбран каталог: $path")
          state.root = Some(path)
          state.current = Some(path)
          state.rootSummary = None
          state.lastProblems = Nil
      catch
        case e: InvalidPathException =>
          println("Не удалось прочитать путь")
          Logger.warn("Некорректные параметры: путь некорректный")
