package diskanalyzer.view

import java.nio.file.Path
import diskanalyzer.{AppConfig, DirSummary, ScanProblem, SizeInfo, SortOrder}

object DirectoryView:

  def renderDiskInfo(info: SizeInfo, config: AppConfig): List[String] =
    List(
      s"Диск: всего ${SizeFormat.format(info.totalBytes, config.sizeUnits)}, " +
        s"занято ${SizeFormat.format(info.usedBytes, config.sizeUnits)}, " +
        s"свободно ${SizeFormat.format(info.freeBytes, config.sizeUnits)}"
    )

  private def formatSize(bytes: Long, complete: Boolean, config: AppConfig): String =
    val size = SizeFormat.format(bytes, config.sizeUnits)
    if complete then size else s"≥ $size"

  def sortedChildren(summary: DirSummary, config: AppConfig): List[DirSummary] =
    config.defaultSort match
      case SortOrder.SizeDescending => summary.children.toList.sortBy(-_.sizeBytes)
      case SortOrder.SizeAscending => summary.children.toList.sortBy(_.sizeBytes)
      case SortOrder.NameAscending => summary.children.toList.sortBy(_.path.getFileName.toString)

  def visibleChildren(summary: DirSummary, config: AppConfig): List[DirSummary] =
    sortedChildren(summary, config).filter(_.sizeBytes >= config.minimumDisplayedSizeBytes)

  private def formatChildLine(
      number: Int,
      dir: DirSummary,
      maxBytes: Long,
      totalBytes: Long,
      config: AppConfig,
      isMarked: Boolean
  ): String =
    val bar = Bar.render(dir.sizeBytes, maxBytes, config.barWidth, config.barCharacter)
    val size = formatSize(dir.sizeBytes, dir.complete, config)
    val percent = if totalBytes > 0 then dir.sizeBytes.toDouble / totalBytes * 100 else 0.0
    val incompleteTag = if dir.complete then "" else "  [неполные данные]"
    val markTag = if isMarked then "  [кандидат]" else ""
    val fullName = dir.path.getFileName.toString
    val name = if fullName.length > 20 then fullName.take(19) + "…" else fullName
    val barColumn = bar + " " * (config.barWidth * config.barCharacter.length - bar.length)
    f"$number%2d. $name%-20s $size%12s   $barColumn ${percent}%5.1f %%$incompleteTag$markTag"

  def renderSummaryHeader(summary: DirSummary, config: AppConfig): List[String] =
    val incompleteNote = if summary.complete then Nil else List("Внимание: результат неполный.")
    List(
      s"Текущий каталог: ${summary.path}",
      s"Размер каталога: ${formatSize(summary.sizeBytes, summary.complete, config)}",
      s"Файлов: ${summary.fileCount}, каталогов: ${summary.dirCount}",
      s"Файлы текущего каталога: ${SizeFormat.format(summary.ownFilesBytes, config.sizeUnits)}"
    ) ++ incompleteNote

  def renderChildrenSection(
      allVisible: List[DirSummary],
      page: List[DirSummary],
      totalBytes: Long,
      config: AppConfig,
      candidatePaths: Set[Path]
  ): List[String] =
    if allVisible.isEmpty then List("(вложенных каталогов нет)")
    else
      val maxBytes = allVisible.map(_.sizeBytes).max
      page.indices.toList.map(i =>
        formatChildLine(i + 1, page(i), maxBytes, totalBytes, config, candidatePaths.contains(page(i).path))
      )

  private def describeProblem(problem: ScanProblem): String =
    problem match
      case ScanProblem.AccessDenied(p) => s"  нет доступа: $p"
      case ScanProblem.ReadError(p, reason) => s"  ошибка чтения: $p ($reason)"
      case ScanProblem.Vanished(p) => s"  исчез во время сканирования: $p"
      case ScanProblem.CyclicLink(p) => s"  циклическая ссылка: $p"
      case ScanProblem.CacheLimitReached(p) => s"  достигнут лимит кэшируемых каталогов, не досканировано: $p"

  def renderProblems(problems: List[ScanProblem], limit: Int = 10): List[String] =
    if problems.isEmpty then Nil
    else
      val lines = problems.take(limit).map(p => describeProblem(p))
      val more = if problems.length > limit then List(s"  ... и ещё ${problems.length - limit}") else Nil
      List(s"Предупреждения (${problems.length}):") ++ lines ++ more

  def spinnerFrames(config: AppConfig): Array[String] =
    val filled = config.progressFilledCharacter
    val empty = config.progressEmptyCharacter
    Array(
      s"$filled$empty$empty",
      s"$empty$filled$empty",
      s"$empty$empty$filled"
    )
