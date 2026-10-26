package diskanalyzer.app

import java.nio.file.Path
import scala.collection.mutable.ListBuffer
import diskanalyzer.{AppConfig, Candidate, DirSummary, ScanProblem, ScanRules}

class AppState(var config: AppConfig, var rules: ScanRules, val configPath: Path):
  var root: Option[Path] = None
  var current: Option[Path] = None
  var rootSummary: Option[DirSummary] = None
  val candidates: ListBuffer[Candidate] = ListBuffer.empty
  var lastProblems: List[ScanProblem] = Nil
  var exitRequested: Boolean = false
  
  def currentSummary: Option[DirSummary] =
    (rootSummary, current) match
      case (Some(summary), Some(cur)) => summary.find(cur)
      case _ => None

  def currentIsMarked: Boolean =
    current match
      case Some(cur) => candidates.exists(_.path == cur)
      case None => false
