package diskanalyzer.command

import java.nio.file.Path
import scala.io.StdIn
import diskanalyzer.app.AppState
import diskanalyzer.{DiskSpace, Logger, ScanProblem, Scanner}
import diskanalyzer.view.DirectoryView

object ScanCommand extends Command:
  val label = "сканировать текущий каталог"

  def execute(state: AppState): Unit = performScan(state, "Сканирование запущено...", "Начато сканирование")

  private def summarizeProblems(problems: List[ScanProblem]): String =
    val accessDenied = problems.count(_.isInstanceOf[ScanProblem.AccessDenied])
    val readErrors = problems.count(_.isInstanceOf[ScanProblem.ReadError])
    val vanished = problems.count(_.isInstanceOf[ScanProblem.Vanished])
    val cyclic = problems.count(_.isInstanceOf[ScanProblem.CyclicLink])
    val cacheLimit = problems.count(_.isInstanceOf[ScanProblem.CacheLimitReached])
    s"недоступно=$accessDenied, ошибокЧтения=$readErrors, исчезло=$vanished, циклическихСсылок=$cyclic, лимитКэша=$cacheLimit"
  
  def performScan(state: AppState, startedMessage: String, logStartLabel: String): Unit =
    (state.root, state.current) match
      case (Some(root), Some(current)) =>
        println(startedMessage)
        println("Нажмите Enter, чтобы прервать сканирование.")
        Logger.info(s"$logStartLabel: path=$current")

        val frames = DirectoryView.spinnerFrames(state.config)
        var tick = 0
        var printedProgress = false

        val onProgress: (Long, Long, Path) => Unit = (dirsDone, filesDone, path) =>
          val frame = frames(tick % frames.length)
          tick += 1
          printedProgress = true
          print(s"\r[$frame] Обработано каталогов: $dirsDone, файлов: $filesDone | $path" + " " * 8)

        val checkCancelled: () => Boolean = () =>
          if System.in.available() > 0 then
            StdIn.readLine()
            true
          else
            false

        val result = Scanner.scan(current, state.rules, onProgress, checkCancelled)
        if printedProgress then println()

        if result.cancelled then
          println("Сканирование прервано пользователем. Результаты неполные.")
          Logger.warn(s"Сканирование прервано: files=${result.summary.fileCount}, directories=${result.summary.dirCount}")
        state.currentSummary match
          case Some(node) => node.updateFrom(result.summary)
          case None => state.rootSummary = Some(result.summary)
        state.lastProblems = result.problems
        DiskSpace.info(root) match
          case Right(info) => DirectoryView.renderDiskInfo(info, state.config).foreach(println)
          case Left(err) => println(s"Не удалось определить ёмкость диска: $err")
        DirectoryView.renderSummaryHeader(result.summary, state.config).foreach(println)
        val warnings = DirectoryView.renderProblems(result.problems)
        if warnings.nonEmpty then warnings.foreach(println)
        if result.problems.nonEmpty then Logger.warn(s"Недоступные объекты при сканировании: ${summarizeProblems(result.problems)}")
        if !result.cancelled then
          println(if result.summary.complete then "Сканирование завершено." else "Сканирование завершено с предупреждениями.")
          Logger.info(s"Сканирование завершено: files=${result.summary.fileCount}, directories=${result.summary.dirCount}")
      case _ =>
        println("Каталог ещё не выбран.")
