package diskanalyzer

import java.io.IOException
import java.nio.charset.CharacterCodingException
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer
import io.circe.Json
import io.circe.parser.parse

enum SizeUnit:
  case Automatic, Bytes, Kilobytes, Megabytes, Gigabytes

enum SortOrder:
  case SizeDescending, SizeAscending, NameAscending

case class AppConfig(
    logFile: String,
    reportFile: String,
    barWidth: Int,
    barCharacter: String,
    progressFilledCharacter: String,
    progressEmptyCharacter: String,
    sizeUnits: SizeUnit,
    defaultSort: SortOrder,
    followSymbolicLinks: Boolean,
    showHiddenDirectories: Boolean,
    maximumCachedDirectories: Int,
    minimumDisplayedSizeBytes: Long
)

object AppConfig:
  val default: AppConfig = AppConfig(
    logFile = "disk-analyzer.log",
    reportFile = "cleanup-candidates.txt",
    barWidth = 20,
    barCharacter = "█",
    progressFilledCharacter = "█",
    progressEmptyCharacter = "░",
    sizeUnits = SizeUnit.Automatic,
    defaultSort = SortOrder.SizeDescending,
    followSymbolicLinks = false,
    showHiddenDirectories = true,
    maximumCachedDirectories = 10000,
    minimumDisplayedSizeBytes = 0L
  )

case class ConfigError(message: String)

object ConfigValidator:

  def validate(json: Json): Either[List[ConfigError], AppConfig] =
    if !json.isObject then Left(List(ConfigError("конфигурационный файл должен быть JSON-объектом")))
    else
      val cursor = json.hcursor
      val errors = ListBuffer.empty[ConfigError]

      def wrongType(key: String): Unit =
        errors += ConfigError(s"поле '$key' имеет неверный тип")

      def str(key: String, default: String): String =
        cursor.getOrElse[String](key)(default) match
          case Right(value) => value
          case Left(_) =>
            wrongType(key)
            default

      def int(key: String, default: Int): Int =
        cursor.getOrElse[Int](key)(default) match
          case Right(value) => value
          case Left(_) =>
            wrongType(key)
            default

      def long(key: String, default: Long): Long =
        cursor.getOrElse[Long](key)(default) match
          case Right(value) => value
          case Left(_) =>
            wrongType(key)
            default

      def bool(key: String, default: Boolean): Boolean =
        cursor.getOrElse[Boolean](key)(default) match
          case Right(value) => value
          case Left(_) =>
            wrongType(key)
            default

      val logFile = str("logFile", "")
      if logFile.isBlank then errors += ConfigError("путь к журналу (logFile) не указан")

      val reportFile = str("reportFile", "")
      if reportFile.isBlank then errors += ConfigError("путь к отчёту (reportFile) не указан")

      val barWidth = int("barWidth", 20)
      if barWidth < 1 then errors += ConfigError("длина полосы (barWidth) должна быть не меньше 1")

      val barCharacter = str("barCharacter", "█")
      if barCharacter.isEmpty then errors += ConfigError("символ полосы (barCharacter) не может быть пустым")

      val progressFilled = str("progressFilledCharacter", "█")
      val progressEmpty = str("progressEmptyCharacter", "░")

      val sizeUnitsRaw = str("sizeUnits", "automatic")
      val sizeUnits = sizeUnitsRaw match
        case "automatic" => SizeUnit.Automatic
        case "bytes" => SizeUnit.Bytes
        case "kilobytes" => SizeUnit.Kilobytes
        case "megabytes" => SizeUnit.Megabytes
        case "gigabytes" => SizeUnit.Gigabytes
        case _ =>
          errors += ConfigError(s"неизвестный способ отображения единиц: '$sizeUnitsRaw'")
          SizeUnit.Automatic

      val sortRaw = str("defaultSort", "sizeDescending")
      val sortOrder = sortRaw match
        case "sizeDescending" => SortOrder.SizeDescending
        case "sizeAscending" => SortOrder.SizeAscending
        case "nameAscending" => SortOrder.NameAscending
        case _ =>
          errors += ConfigError(s"неизвестный порядок сортировки: '$sortRaw'")
          SortOrder.SizeDescending

      val followSymlinks = bool("followSymbolicLinks", false)
      val showHidden = bool("showHiddenDirectories", true)

      val maxCached = int("maximumCachedDirectories", 10000)
      if maxCached < 1 then
        errors += ConfigError("maximumCachedDirectories должно быть не меньше 1")

      val minSize = long("minimumDisplayedSizeBytes", 0L)
      if minSize < 0 then
        errors += ConfigError("minimumDisplayedSizeBytes не может быть отрицательным")

      if errors.nonEmpty then Left(errors.toList)
      else
        Right(
          AppConfig(
            logFile,
            reportFile,
            barWidth,
            barCharacter,
            progressFilled,
            progressEmpty,
            sizeUnits,
            sortOrder,
            followSymlinks,
            showHidden,
            maxCached,
            minSize
          )
        )

object ConfigLoader:
  def loadFromFile(path: Path): Either[List[ConfigError], AppConfig] =
    if !Files.exists(path) then Left(List(ConfigError(s"файл конфигурации не найден: $path")))
    else
      try
        val text = Files.readString(path)
        parse(text) match
          case Left(err) => Left(List(ConfigError(s"конфигурационный файл имеет неправильный формат: ${err.message}")))
          case Right(json) => ConfigValidator.validate(json)
      catch
        case _: CharacterCodingException =>
          Left(List(ConfigError(s"файл конфигурации должен быть сохранён в кодировке UTF-8: $path")))
        case e: IOException =>
          Left(List(ConfigError(s"не удалось прочитать файл конфигурации: ${e.getMessage}")))
