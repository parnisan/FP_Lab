package diskanalyzer

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

case class ScanRules(followSymbolicLinks: Boolean, showHiddenDirectories: Boolean, maximumCachedDirectories: Int)

case class ScanResult(summary: DirSummary, problems: List[ScanProblem], cancelled: Boolean)

object Scanner:
  def scan(
      startDir: Path,
      rules: ScanRules,
      onProgress: (Long, Long, Path) => Unit,
      checkCancelled: () => Boolean
  ): ScanResult =
    val problems = ListBuffer.empty[ScanProblem]
    val visitedRealPaths = scala.collection.mutable.Set.empty[Path]
    var cachedDirs = 0L
    var dirsDone = 0L
    var filesDone = 0L
    var cancelled = false
    val scannedAt = LocalDateTime.now()

    def scanDir(dir: Path): DirSummary =
      cachedDirs += 1
      if cachedDirs > rules.maximumCachedDirectories then
        problems += ScanProblem.CacheLimitReached(dir)
        DirSummary(path = dir, ownFilesBytes = 0L, ownFileCount = 0L, ownComplete = false, scannedAt = scannedAt, children = ListBuffer.empty)
      else
        onProgress(dirsDone, filesDone, dir)
        var ownFilesBytes = 0L
        var ownFileCount = 0L
        var ownOk = true
        var stoppedEarly = false
        val children = ListBuffer.empty[DirSummary]

        def classifyAndVisit(entry: Path): Unit =
          try
            val attrs = Files.readAttributes(entry, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS)
            val isLink = attrs.isSymbolicLink || attrs.isOther
            val hidden = Files.isHidden(entry)

            if hidden && !rules.showHiddenDirectories then
              ()
            else if attrs.isDirectory && !isLink then
              children += scanDir(entry)
            else if isLink then
              if rules.followSymbolicLinks then
                val real = entry.toRealPath()
                if visitedRealPaths.contains(real) then
                  problems += ScanProblem.CyclicLink(entry)
                  ownOk = false
                else
                  visitedRealPaths += real
                  children += scanDir(entry)
            else
              ownFileCount += 1
              ownFilesBytes += attrs.size()
              filesDone += 1
              if filesDone % 200 == 0 then onProgress(dirsDone, filesDone, entry)
          catch
            case _: NoSuchFileException =>
              problems += ScanProblem.Vanished(entry)
              ownOk = false
            case _: AccessDeniedException =>
              problems += ScanProblem.AccessDenied(entry)
              ownOk = false
            case e: IOException =>
              problems += ScanProblem.ReadError(entry, Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
              ownOk = false

        try
          val stream = Files.newDirectoryStream(dir)
          try
            val it = stream.iterator()
            while it.hasNext && !cancelled do
              if checkCancelled() then cancelled = true
              else classifyAndVisit(it.next())
            if cancelled then stoppedEarly = true
          finally stream.close()
        catch
          case _: AccessDeniedException =>
            problems += ScanProblem.AccessDenied(dir)
            ownOk = false
          case e: IOException =>
            problems += ScanProblem.ReadError(dir, Option(e.getMessage).getOrElse(e.getClass.getSimpleName))
            ownOk = false

        if !stoppedEarly then dirsDone += 1

        DirSummary(
          path = dir,
          ownFilesBytes = ownFilesBytes,
          ownFileCount = ownFileCount,
          ownComplete = ownOk && !stoppedEarly,
          scannedAt = scannedAt,
          children = children
        )

    val summary = scanDir(startDir)
    ScanResult(summary, problems.toList, cancelled)

object DiskSpace:
  def info(path: Path): Either[String, SizeInfo] =
    try
      val store = Files.getFileStore(path)
      val total = store.getTotalSpace
      val usable = store.getUsableSpace
      Right(SizeInfo(totalBytes = total, usedBytes = total - usable, freeBytes = usable))
    catch
      case e: IOException => Left(Option(e.getMessage).getOrElse("не удалось определить ёмкость диска"))
