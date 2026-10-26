package diskanalyzer.command

import java.nio.file.{Files, Paths}
import diskanalyzer.app.AppState
import diskanalyzer.{Candidates, Logger, ReportBuilder, ReportWriter}

object ReportCommand extends Command:
  val label = "сформировать отчёт"

  def execute(state: AppState): Unit =
    if state.candidates.isEmpty then
      println("Список кандидатов пуст — сначала отметьте хотя бы один каталог (пункт 'mark').")
    else
      val ordered = state.candidates.toList
      val survivors = Candidates.withoutNested(ordered.filter(c => Files.exists(c.path)))
      val freedBytes = survivors.map(_.sizeBytes).sum
      val freedComplete = survivors.forall(_.complete)
      val lines = ReportBuilder.build(ordered, freedBytes, freedComplete, state.config)
      val reportPath = Paths.get(state.config.reportFile)
      ReportWriter.write(reportPath, lines) match
        case Right(_) =>
          println(s"Отчёт сохранён: ${reportPath.toAbsolutePath}")
          Logger.info(s"Сформирован отчёт: candidates=${ordered.length}")
        case Left(err) =>
          println(s"Не удалось сохранить отчёт: $err")
          Logger.error(s"Не удалось сохранить отчёт: $err")
