package diskanalyzer.command

import java.nio.file.Path
import scala.io.StdIn
import diskanalyzer.app.AppState
import diskanalyzer.{Candidate, Logger}

object MarkCommand extends Command:
  val label = "отметить/снять отметку текущего каталога"

  private def confirmOverlap(path: Path, other: Candidate, newIsInside: Boolean): Boolean =
    println()
    println("Каталог")
    println()
    println(path.toString)
    println()
    println(if newIsInside then "уже входит в отмеченный каталог" else "уже содержит отмеченный каталог")
    println()
    println(other.path.toString)
    println()
    val answer = StdIn.readLine("Оставить обе отметки? [да/нет]: ")
    answer != null && Set("да", "д", "yes", "y").contains(answer.trim.toLowerCase)

  def execute(state: AppState): Unit =
    state.currentSummary match
      case None =>
        println("Сначала выполните сканирование (пункт 'scan').")
      case Some(current) =>
        state.candidates.find(_.path == current.path) match
          case Some(existing) =>
            state.candidates -= existing
            println(s"Отметка снята: ${current.path}")
            Logger.info(s"Удалён кандидат: sizeBytes=${current.sizeBytes}")
          case None =>
            val overlap = state.candidates.find(c => current.path.startsWith(c.path) || c.path.startsWith(current.path))
            val proceed = overlap match
              case None => true
              case Some(other) =>
                Logger.warn("Обнаружено пересечение кандидатов")
                val newIsInside = current.path.startsWith(other.path)
                val keepBoth = confirmOverlap(current.path, other, newIsInside)
                if !keepBoth then println("Отметка отменена.")
                keepBoth
            if proceed then
              val commentRaw = StdIn.readLine("Комментарий (необязательно, Enter чтобы пропустить): ")
              val comment = if commentRaw == null || commentRaw.isBlank then None else Some(commentRaw.trim)
              val candidate = Candidate(
                path = current.path,
                sizeBytes = current.sizeBytes,
                complete = current.complete,
                scannedAt = current.scannedAt,
                comment = comment
              )
              println(s"Каталог отмечен как кандидат: ${current.path}")
              Logger.info(s"Добавлен кандидат: sizeBytes=${candidate.sizeBytes}")
              state.candidates += candidate
