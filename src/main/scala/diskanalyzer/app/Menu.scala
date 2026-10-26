package diskanalyzer.app

import diskanalyzer.command.*
import diskanalyzer.Logger
import diskanalyzer.view.SizeFormat

class Menu:

  private val items: List[Command] =
    List(
      ScanCommand,
      SelectCommand,
      OpenCommand,
      MarkCommand,
      BackCommand,
      CandidatesCommand,
      ReportCommand,
      RescanCommand,
      ProblemsCommand,
      ReloadCommand
    )

  def run(state: AppState): Unit =
    while !state.exitRequested do
      println()
      statusLines(state).foreach(println)
      println()
      for i <- items.indices do
        println(f"${i + 1}%2d. ${items(i).label}")
      println(" 0. выход")

      val line = scala.io.StdIn.readLine("Выберите пункт: ")
      if line == null then
        state.exitRequested = true
      else
        line.trim.toIntOption match
          case Some(0) =>
            ExitCommand.execute(state)
          case Some(n) if n >= 1 && n <= items.length =>
            try
              items(n - 1).execute(state)
            catch
              case e: Exception =>
                println(s"Внутренняя ошибка: ${e.getMessage}")
                Logger.error(s"Внутренняя ошибка при выполнении '${items(n - 1).label}': ${e.getMessage}")
          case _ =>
            println("Некорректный выбор, попробуйте ещё раз.")
            Logger.warn(s"Некорректный выбор пункта меню: '$line'")

  private def statusLines(state: AppState): List[String] =
    val current = state.current match
      case Some(path) => path.toString
      case None => "(не выбран)"
    val markTag = if state.currentIsMarked then "  [кандидат]" else ""
    val scanTag = state.currentSummary match
      case Some(summary) =>
        val size = SizeFormat.format(summary.sizeBytes, state.config.sizeUnits)
        val incompleteTag = if summary.complete then "" else " (неполные данные)"
        s"Размер: $size$incompleteTag, файлов: ${summary.fileCount}, каталогов: ${summary.dirCount}"
      case None =>
        "(ещё не сканировался — выберите пункт 'scan')"
    List(s"Текущий каталог: $current$markTag", scanTag)
