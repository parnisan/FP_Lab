package diskanalyzer

import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

enum LogLevel:
  case Info, Warn, Error

object Logger:
  private val timeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  private var logPath: Option[Path] = None

  def init(path: Path): Unit = logPath = Some(path)

  def info(message: String): Unit = write(LogLevel.Info, message)
  def warn(message: String): Unit = write(LogLevel.Warn, message)
  def error(message: String): Unit = write(LogLevel.Error, message)

  private def levelLabel(level: LogLevel): String = level match
    case LogLevel.Info => "INFO "
    case LogLevel.Warn => "WARN "
    case LogLevel.Error => "ERROR"

  private def write(level: LogLevel, message: String): Unit =
    logPath.foreach { path =>
      try
        val timestamp = LocalDateTime.now().format(timeFormat)
        val line = s"$timestamp ${levelLabel(level)} $message${System.lineSeparator()}"
        Files.writeString(path, line, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      catch
        case _: Exception => ()
    }
