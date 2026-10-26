package diskanalyzer

import java.io.IOException
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import diskanalyzer.view.SizeFormat

object ReportBuilder:
  private val dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def build(candidates: List[Candidate], freedBytes: Long, freedComplete: Boolean, config: AppConfig): List[String] =
    val freedText = SizeFormat.format(freedBytes, config.sizeUnits)
    val freedLine = if freedComplete then freedText else s"не менее $freedText"
    val header = List(
      s"Дата формирования: ${LocalDateTime.now().format(dateFormat)}",
      s"Кандидатов: ${candidates.length}",
      s"Потенциально освобождаемое место: $freedLine",
      ""
    )
    header ++ candidates.map(_.path.toString)

object ReportWriter:
  def write(path: Path, lines: List[String]): Either[String, Unit] =
    try
      Files.writeString(path, lines.mkString("\n") + "\n")
      Right(())
    catch
      case e: IOException => Left(Option(e.getMessage).getOrElse("не удалось записать файл отчёта"))
