package diskanalyzer

import java.nio.file.Path
import java.time.LocalDateTime
import scala.collection.mutable.ListBuffer

class DirSummary(
    val path: Path,
    var ownFilesBytes: Long,
    var ownFileCount: Long,
    var ownComplete: Boolean,
    var scannedAt: LocalDateTime,
    val children: ListBuffer[DirSummary]
):
  def sizeBytes: Long = ownFilesBytes + children.map(_.sizeBytes).sum
  def fileCount: Long = ownFileCount + children.map(_.fileCount).sum
  def dirCount: Long = children.length + children.map(_.dirCount).sum
  def complete: Boolean = ownComplete && children.forall(_.complete)
  
  def find(target: Path): Option[DirSummary] =
    if path == target then Some(this)
    else
      var found: Option[DirSummary] = None
      for child <- children do
        if found.isEmpty && target.startsWith(child.path) then
          found = child.find(target)
      found
  
  def updateFrom(fresh: DirSummary): Unit =
    ownFilesBytes = fresh.ownFilesBytes
    ownFileCount = fresh.ownFileCount
    ownComplete = fresh.ownComplete
    scannedAt = fresh.scannedAt
    children.clear()
    children ++= fresh.children

enum ScanProblem:
  case AccessDenied(path: Path)
  case ReadError(path: Path, reason: String)
  case Vanished(path: Path)
  case CyclicLink(path: Path)
  case CacheLimitReached(path: Path)

case class Candidate(
    path: Path,
    sizeBytes: Long,
    complete: Boolean,
    scannedAt: LocalDateTime,
    comment: Option[String]
)

object Candidates:

  private def isNestedInAnother(candidate: Candidate, all: List[Candidate]): Boolean =
    all.exists(other => other != candidate && candidate.path.startsWith(other.path))

  def withoutNested(all: List[Candidate]): List[Candidate] =
    all.filterNot(c => isNestedInAnother(c, all))

  def totalBytes(all: List[Candidate]): Long =
    all.map(_.sizeBytes).sum

case class SizeInfo(totalBytes: Long, usedBytes: Long, freeBytes: Long)
