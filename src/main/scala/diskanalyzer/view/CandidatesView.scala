package diskanalyzer.view

import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import diskanalyzer.{AppConfig, Candidate, Candidates}

object CandidatesView:

  def render(candidates: List[Candidate], config: AppConfig): List[String] =
    if candidates.isEmpty then List("Кандидатов нет.")
    else
      val lines = ListBuffer.empty[String]
      lines += "Кандидаты на удаление:"
      lines += ""

      var number = 1
      var missingCount = 0
      var completeCount = 0
      var incompleteCount = 0
      for candidate <- candidates do
        val exists = Files.exists(candidate.path)
        val sizeText = SizeFormat.format(candidate.sizeBytes, config.sizeUnits)
        val size = if candidate.complete then sizeText else s"не менее $sizeText"
        var state = ""
        if !exists then
          state = "каталог больше не существует"
          missingCount += 1
        else if candidate.complete then
          state = "просканирован полностью"
          completeCount += 1
        else
          state = "часть файлов недоступна"
          incompleteCount += 1

        lines += s"$number. ${candidate.path}"
        lines += s"   Размер: $size"
        lines += s"   Состояние: $state"
        candidate.comment match
          case Some(c) => lines += s"   Комментарий: $c"
          case None => ()
        lines += ""
        number += 1

      val survivors = Candidates.withoutNested(candidates.filter(c => Files.exists(c.path)))
      val freedBytes = survivors.map(_.sizeBytes).sum
      val freedComplete = survivors.forall(_.complete)
      val freedText = SizeFormat.format(freedBytes, config.sizeUnits)
      val freedLine = if freedComplete then freedText else s"не менее $freedText"

      lines += s"Кандидатов: ${candidates.length}"
      lines += s"Сумма размеров всех кандидатов: ${SizeFormat.format(Candidates.totalBytes(candidates), config.sizeUnits)}"
      lines += s"Потенциально освобождаемое место: $freedLine"
      lines += s"Просканировано полностью: $completeCount, не полностью: $incompleteCount"
      lines += s"Больше не существует: $missingCount"
      lines.toList
